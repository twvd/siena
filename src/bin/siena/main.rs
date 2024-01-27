use std::fs;
use std::sync::Arc;
use std::thread;
use std::time::SystemTime;

use anyhow::{Context, Result};
use clap::Parser;
use colored::*;
use sdl2::event::Event;
use sdl2::keyboard::Keycode;
use serde::Deserialize;
use serde_json::Deserializer;

use siena::frontend::channel::ChannelRenderer;
use siena::frontend::gif::Gif;
use siena::frontend::sdl::{SDLAudioSink, SDLEventPump, SDLRenderer};
use siena::frontend::Renderer;
use siena::snes::bus::mainbus::{BusTrace, Mainbus};
use siena::snes::bus::Bus;
use siena::snes::cartridge::{Cartridge, VideoFormat};
use siena::snes::cpu_65816::cpu::Cpu65816;
use siena::snes::joypad::{Button, Joypad, JoypadEvent};
use siena::snes::ppu::ppu::{SCREEN_HEIGHT, SCREEN_WIDTH};

/// Maps an SDL keycode to a controller input for a specific controller.
fn map_keycode(keycode: Keycode) -> Option<(usize, Button)> {
    match keycode {
        Keycode::A => Some((0, Button::A)),
        Keycode::B => Some((0, Button::B)),
        Keycode::X => Some((0, Button::X)),
        Keycode::Y => Some((0, Button::Y)),
        Keycode::L => Some((0, Button::L)),
        Keycode::R => Some((0, Button::R)),
        Keycode::Space => Some((0, Button::Start)),
        Keycode::Period => Some((0, Button::Select)),
        Keycode::Up => Some((0, Button::Up)),
        Keycode::Down => Some((0, Button::Down)),
        Keycode::Left => Some((0, Button::Left)),
        Keycode::Right => Some((0, Button::Right)),

        _ => None,
    }
}

/// Signals the main thread can send to the emulation thread.
enum EmuThreadSignal {
    Quit,
    DumpState,
    ToggleVerbose,
    ToggleVerboseSPC,
}

#[derive(Parser)]
#[command(
    about = "Siena - SNES Emulator",
    author = "Thomas <thomas@thomasw.dev>",
    long_about = None)]
struct Args {
    /// ROM filename to load.
    filename: String,

    /// Wait for keystroke after each CPU step.
    #[arg(short, long)]
    pause: bool,

    /// Print S-CPU state after each instruction
    #[arg(short, long)]
    verbose: bool,

    /// Print SPC700 state after each instruction
    #[arg(long)]
    spc_verbose: bool,

    /// Sticky joypad controls
    #[arg(long)]
    sticky: bool,

    /// S-CPU bus trace mode
    #[arg(
        long,
        require_equals = true,
        value_name = "MODE",
        num_args = 0..=1,
        default_value_t = BusTrace::None,
        default_missing_value = "none",
        value_enum
    )]
    trace_bus: BusTrace,

    /// S-CPU <-> APU communication trace
    #[arg(long)]
    trace_apu_comm: bool,

    /// Load state file
    #[arg(long)]
    state: Option<String>,

    /// Skip cartridge header detection, load as LoROM (mostly for test ROMs)
    #[arg(long)]
    no_header: bool,

    /// Skip cartridge header detection, load as HiROM (mostly for test ROMs)
    #[arg(long)]
    no_header_hirom: bool,

    /// Override frame rate limit (0 = unlimited)
    #[arg(long)]
    fps: Option<u64>,

    /// Override video format
    #[arg(long)]
    videoformat: Option<VideoFormat>,

    /// Co-processor ROM to load (if needed)
    #[arg(short, long)]
    corom: Option<String>,

    /// SPC700 (APU) IPL to load
    #[arg(long, default_value = "spc700.rom")]
    spc_ipl: String,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Set up joypad inputs
    let (mut joypads, joypad_senders) = Joypad::new_channel_all();
    for j in joypads.iter_mut() {
        j.sticky_enabled = args.sticky;
    }

    // Set up the display and events
    let mut display = SDLRenderer::new(SCREEN_WIDTH, SCREEN_HEIGHT)?;
    let mut displaychannel = ChannelRenderer::new(SCREEN_WIDTH, SCREEN_HEIGHT)?;
    let framereceiver = displaychannel.get_receiver();
    let eventpump = SDLEventPump::new();

    // Initialize cartridge
    let f = fs::read(args.filename)?;
    let f_co = if let Some(filename) = args.corom {
        Some(fs::read(filename)?)
    } else {
        None
    };
    let cartridge = if !args.no_header && !args.no_header_hirom {
        let c = Cartridge::load(&f, f_co.as_deref());
        println!("Cartridge: {}", &c);
        c
    } else {
        Cartridge::load_nohdr(&f, args.no_header_hirom)
    };
    let fn_title = cartridge.get_title_clean();

    // Determine video format (PAL/NTSC)
    let videoformat = match args.videoformat {
        Some(f) => f,
        None => cartridge.get_video_format(),
    };

    // Determine frame rate limit, either based on the video format
    // or what the user specified.
    let fps = match args.fps {
        None => match videoformat {
            VideoFormat::NTSC => 60,
            VideoFormat::PAL => 50,
        },
        Some(fps) => fps,
    };

    // Load SPC700 IPL ROM
    let apu_ipl = fs::read(&args.spc_ipl)
        .with_context(|| format!("Failed to load SPC700 IPL ROM from {}", &args.spc_ipl))?;

    // Initialize S-CPU bus
    let mut bus = Mainbus::<ChannelRenderer>::new(
        cartridge,
        args.trace_bus,
        displaychannel,
        joypads,
        &apu_ipl,
        args.verbose,
        fps,
        videoformat,
    );

    // Initialize audio
    let _audio = SDLAudioSink::init(bus.get_apu());

    #[cfg(not(feature = "apu_blargg"))]
    {
        let mut apu = bus.apu.lock().unwrap();
        apu.verbose = args.spc_verbose;
        apu.ports.write().unwrap().trace = args.trace_apu_comm;
    }

    // Fetch reset vector address
    let reset = bus.read16(0xFFFC);
    println!("Reset at PC {:06X}", reset);

    // Initialize S-CPU
    let mut cpu = Cpu65816::<Mainbus<ChannelRenderer>>::new(bus, reset);

    // Load and deserialize state file
    if let Some(state_filename) = args.state {
        println!("Restoring state from {}", state_filename);
        let json = fs::read_to_string(state_filename)?;
        let mut deserializer = Deserializer::from_str(&json);

        // TODO Pending https://github.com/serde-rs/serde/issues/2512
        //Deserialize::deserialize_in_place(&mut deserializer, &mut cpu)?;

        // Until then..
        let mut new_cpu: Cpu65816<Mainbus<ChannelRenderer>> =
            Deserialize::deserialize(&mut deserializer)?;
        // ..and move all the non-serializable stuff over.
        new_cpu.bus.ppu.renderer = std::mem::replace(&mut cpu.bus.ppu.renderer, None);
        new_cpu.bus.joypads = std::mem::replace(&mut cpu.bus.joypads, None);

        cpu = new_cpu;
    }

    // Spin up emulation thread and communication channel
    let (emuthread_tx, emuthread_rx) = crossbeam_channel::unbounded();
    let mut t_verbose = args.verbose;
    let emuthread = thread::spawn(move || loop {
        // Handle signals from main thread
        match emuthread_rx.try_recv() {
            Ok(EmuThreadSignal::Quit) => break,
            Ok(EmuThreadSignal::DumpState) => {
                let filename = format!(
                    "state_{}.json",
                    SystemTime::now()
                        .duration_since(SystemTime::UNIX_EPOCH)
                        .expect("Timetravel detected")
                        .as_secs()
                );
                let file = fs::File::create(&filename).unwrap();
                serde_json::to_writer(file, &cpu).unwrap();
                println!("State dumped to {}", filename);
            }
            Ok(EmuThreadSignal::ToggleVerbose) => t_verbose = !t_verbose,
            #[cfg(not(feature = "apu_blargg"))]
            Ok(EmuThreadSignal::ToggleVerboseSPC) => {
                let mut apu = cpu.bus.apu.lock().unwrap();
                apu.verbose = !apu.verbose;
            }
            _ => (),
        }

        // Step the CPU
        if t_verbose {
            println!("{}", cpu.dump_state().green());
        }
        cpu.step().unwrap();
    });

    // Presentation / event thread below
    let mut recording: Option<Gif> = None;
    'mainloop: loop {
        let frame = framereceiver.recv()?;
        display.update_from(Arc::clone(&frame))?;
        if let Some(rec) = recording.as_mut() {
            rec.add(&frame)?;
        }

        while let Some(event) = eventpump.poll() {
            match event {
                // Application exit
                Event::KeyDown {
                    keycode: Some(Keycode::Escape),
                    ..
                }
                | Event::Quit { .. } => break 'mainloop,

                // Dump state
                Event::KeyDown {
                    keycode: Some(Keycode::D),
                    ..
                } => {
                    emuthread_tx.send(EmuThreadSignal::DumpState)?;
                }

                // Toggle S-CPU verbose
                Event::KeyDown {
                    keycode: Some(Keycode::Num0),
                    ..
                } => {
                    emuthread_tx.send(EmuThreadSignal::ToggleVerbose)?;
                }

                // Toggle SPC700 verbose
                Event::KeyDown {
                    keycode: Some(Keycode::Num9),
                    ..
                } => {
                    emuthread_tx.send(EmuThreadSignal::ToggleVerboseSPC)?;
                }

                // Start/stop recording
                Event::KeyDown {
                    keycode: Some(Keycode::Q),
                    ..
                } => {
                    if recording.is_none() {
                        fs::create_dir_all("recordings/")?;
                        let filename = format!(
                            "recordings/{}_{}.gif",
                            fn_title,
                            SystemTime::now()
                                .duration_since(SystemTime::UNIX_EPOCH)
                                .expect("Timetravel detected")
                                .as_secs()
                        );
                        recording = Some(Gif::new(
                            SCREEN_WIDTH,
                            SCREEN_HEIGHT,
                            fps,
                            fs::File::create(&filename)?,
                        )?);
                        println!("Started recording to {}", filename);
                    } else {
                        recording = None;
                        println!("Recording finished");
                    }
                }

                // Controller input
                Event::KeyDown {
                    keycode: Some(k), ..
                } => {
                    if let Some((padidx, button)) = map_keycode(k) {
                        joypad_senders[padidx].send(JoypadEvent::Down(button))?;
                    }
                }
                Event::KeyUp {
                    keycode: Some(k), ..
                } => {
                    if let Some((padidx, button)) = map_keycode(k) {
                        joypad_senders[padidx].send(JoypadEvent::Up(button))?;
                    }
                }
                _ => (),
            }
        }
    }

    emuthread_tx.send(EmuThreadSignal::Quit)?;
    emuthread.join().unwrap();
    Ok(())
}
