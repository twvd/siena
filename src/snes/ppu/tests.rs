use crate::bus::BusMember;
use crate::frontend::NullRenderer;
use crate::snes::cartridge::VideoFormat;

use super::ppu::PPU;
use super::state::PPUState;

fn ppustate() -> PPUState {
    PPUState::new()
}
fn ppu() -> PPU<NullRenderer> {
    PPU::new(NullRenderer {}, 0, VideoFormat::PAL)
}

#[test]
fn vram_write_inc_low() {
    let mut p = ppu();
    p.write(0x2116, 0x34); // VMADDH
    p.write(0x2117, 0x12); // VMADDL
    p.write(0x2115, 0x00); // VMAIN - low 1 word

    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2119, 0xAA); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2118, 0xAA); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x1235);
    p.write(0x2119, 0xBB); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1235);
    p.write(0x2118, 0xBB); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x1236);

    assert_eq!(p.vram[0x1233], 0x0000);
    assert_eq!(p.vram[0x1234], 0xAAAA);
    assert_eq!(p.vram[0x1235], 0xBBBB);
    assert_eq!(p.vram[0x1236], 0x0000);
}

#[test]
fn vram_write_inc_high() {
    let mut p = ppu();
    p.write(0x2116, 0x34); // VMADDH
    p.write(0x2117, 0x12); // VMADDL
    p.write(0x2115, 0x80); // VMAIN - high 1 word

    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2118, 0xAA); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2119, 0xAA); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1235);
    p.write(0x2118, 0xBB); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x1235);
    p.write(0x2119, 0xBB); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1236);

    assert_eq!(p.vram[0x1233], 0x0000);
    assert_eq!(p.vram[0x1234], 0xAAAA);
    assert_eq!(p.vram[0x1235], 0xBBBB);
    assert_eq!(p.vram[0x1236], 0x0000);
}

#[test]
fn vram_write_inc_high_2() {
    let mut p = ppu();
    p.write(0x2116, 0x34); // VMADDH
    p.write(0x2117, 0x12); // VMADDL
    p.write(0x2115, 0x80); // VMAIN - high 1 word

    p.vram[0x1234] = 0x1111;
    p.vram[0x1235] = 0x2222;

    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2119, 0xAA); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1235);
    p.write(0x2119, 0xBB); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1236);

    assert_eq!(p.vram[0x1234], 0xAA11);
    assert_eq!(p.vram[0x1235], 0xBB22);
}

#[test]
fn vram_write_inc_thirtytwo() {
    let mut p = ppu();
    p.write(0x2116, 0x34); // VMADDH
    p.write(0x2117, 0x12); // VMADDL
    p.write(0x2115, 0x01); // VMAIN - low 32 words

    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2119, 0xAA); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2118, 0xAA); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x1254);
    p.write(0x2119, 0xBB); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1254);
    p.write(0x2118, 0xBB); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x1274);

    assert_eq!(p.vram[0x1233], 0x0000);
    assert_eq!(p.vram[0x1234], 0xAAAA);
    assert_eq!(p.vram[0x1235], 0x0000);
    assert_eq!(p.vram[0x1253], 0x0000);
    assert_eq!(p.vram[0x1254], 0xBBBB);
    assert_eq!(p.vram[0x1255], 0x0000);
    assert_eq!(p.vram[0x1274], 0x0000);
}

#[test]
fn vram_write_inc_hundredtwentyeight() {
    let mut p = ppu();
    p.write(0x2116, 0x34); // VMADDH
    p.write(0x2117, 0x12); // VMADDL
    p.write(0x2115, 0x02); // VMAIN - low 128 words

    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2119, 0xAA); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x1234);
    p.write(0x2118, 0xAA); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x12B4);
    p.write(0x2119, 0xBB); // VMDATAH
    assert_eq!(p.vmadd.get(), 0x12B4);
    p.write(0x2118, 0xBB); // VMDATAL
    assert_eq!(p.vmadd.get(), 0x1334);

    assert_eq!(p.vram[0x1233], 0x0000);
    assert_eq!(p.vram[0x1234], 0xAAAA);
    assert_eq!(p.vram[0x1235], 0x0000);
    assert_eq!(p.vram[0x12B3], 0x0000);
    assert_eq!(p.vram[0x12B4], 0xBBBB);
    assert_eq!(p.vram[0x12B5], 0x0000);
    assert_eq!(p.vram[0x1334], 0x0000);
}

#[test]
fn cgram_write() {
    let mut p = ppustate();
    p.write(0x2121, 0); // CGADD
    p.write(0x2122, 0xAA); // CGDATA
    assert_eq!(p.cgadd.get(), 0);
    p.write(0x2122, 0xBB); // CGDATA
    assert_eq!(p.cgadd.get(), 1);
    assert_eq!(p.cgram[0], 0xBBAA);
}

#[test]
fn cgram_addr_reset_flipflop() {
    let mut p = ppustate();
    p.write(0x2121, 0); // CGADD
    p.write(0x2122, 0xAA); // CGDATA
    p.write(0x2121, 0); // CGADD
    p.write(0x2122, 0xBB); // CGDATA
    assert_eq!(p.cgram[0], 0xBB);
}

#[test]
fn cgram_write_overflow() {
    let mut p = ppustate();
    p.write(0x2121, 0xFF); // CGADD
    p.write(0x2122, 0xAA); // CGDATA
    p.write(0x2122, 0xBB); // CGDATA
    p.write(0x2122, 0xCC); // CGDATA
    p.write(0x2122, 0xDD); // CGDATA
    assert_eq!(p.cgram[0], 0xDDCC);
    assert_eq!(p.cgram[0xFF], 0xBBAA);
}

#[test]
fn cgram_read() {
    let mut p = ppustate();
    p.cgram[0] = 0xBBAA;
    p.cgram[1] = 0xDDCC;
    p.write(0x2121, 0); // CGADD
    assert_eq!(p.read(0x213B), Some(0xAA)); // RDCGRAM
    assert_eq!(p.read(0x213B), Some(0xBB)); // RDCGRAM
    assert_eq!(p.read(0x213B), Some(0xCC)); // RDCGRAM
    assert_eq!(p.read(0x213B), Some(0xDD)); // RDCGRAM
}

#[test]
fn cgram_read_overflow() {
    let mut p = ppustate();
    p.cgram[255] = 0xBBAA;
    p.cgram[0] = 0xDDCC;
    p.write(0x2121, 0xFF); // CGADD
    assert_eq!(p.read(0x213B), Some(0xAA)); // RDCGRAM
    assert_eq!(p.read(0x213B), Some(0xBB)); // RDCGRAM
    assert_eq!(p.read(0x213B), Some(0xCC)); // RDCGRAM
    assert_eq!(p.read(0x213B), Some(0xDD)); // RDCGRAM
}

#[test]
fn oam_write_seq() {
    let mut p = ppustate();
    p.write(0x2102, 0);
    p.write(0x2103, 0);
    assert_eq!(p.oam[0..3], [0, 0, 0]);

    // First write buffers
    p.write(0x2104, 0xAA);
    assert_eq!(p.oam[0..3], [0, 0, 0]);
    p.write(0x2104, 0xBB);
    assert_eq!(p.oam[0..3], [0xAA, 0xBB, 0]);

    p.write(0x2104, 0xCC);
    assert_eq!(p.oam[0..3], [0xAA, 0xBB, 0]);
    p.write(0x2104, 0xDD);
    assert_eq!(p.oam[0..5], [0xAA, 0xBB, 0xCC, 0xDD, 0]);

    // Test end of OAM towards the 32 byte extension table
    p.write(0x2102, ((510 >> 1) & 0xFF) as u8);
    p.write(0x2103, ((510 >> 1) >> 8) as u8);
    assert_eq!(p.oam[510..513], [0, 0, 0]);
    p.write(0x2104, 0xAA);
    assert_eq!(p.oam[510..513], [0, 0, 0]);
    p.write(0x2104, 0xBB);
    assert_eq!(p.oam[510..513], [0xAA, 0xBB, 0]);
    p.write(0x2104, 0xCC); // passes immediately
    assert_eq!(p.oam[510..513], [0xAA, 0xBB, 0xCC]);
}

#[test]
fn oam_write_exttable() {
    let mut p = ppustate();
    assert_eq!(p.oam[512], 0);
    p.write(0x2102, ((512 >> 1) & 0xFF) as u8);
    p.write(0x2103, ((512 >> 1) >> 8) as u8);
    p.write(0x2104, 0xAA);
    assert_eq!(p.oam[512], 0xAA);
}

#[test]
fn oam_write_exttable_mirror() {
    let mut p = ppustate();
    assert_eq!(p.oam[512], 0);
    p.write(0x2102, (0x120 & 0xFF) as u8);
    p.write(0x2103, (0x120 >> 8) as u8);
    p.write(0x2104, 0xAA);
    assert_eq!(p.oam[512], 0xAA);
}

#[test]
fn signed_mul() {
    let mut p = ppustate();
    p.write(0x211B, 0x22); // M7A
    p.write(0x211B, 0x11); // M7A
    p.write(0x211C, 0x33); // M7B
    let mut result = p.read(0x2134).unwrap() as i32;
    result |= (p.read(0x2135).unwrap() as i32) << 8;
    result |= (p.read(0x2136).unwrap() as i32) << 16;
    assert_eq!(result, (0x1122_i32 * 0x33_i32));
}

#[test]
fn oam_read() {
    let mut p = ppustate();
    p.oam[3] = 1;
    p.oam[4] = 2;
    p.oam[5] = 3;
    p.write(0x2102, 3 >> 1);
    p.write(0x2103, 0);
    assert_eq!(p.read(0x2138), Some(0));
    assert_eq!(p.read(0x2138), Some(1));
    assert_eq!(p.read(0x2138), Some(2));
    assert_eq!(p.read(0x2138), Some(3));
}

#[test]
fn vram_addr_translate() {
    let mut p = ppu();
    // No translation
    assert_eq!(p.vram_addr_translate(0b0000000011100000), 0b0011100000);
    assert_eq!(p.vram_addr_translate(0b0000000111000001), 0b0111000001);
    assert_eq!(p.vram_addr_translate(0b0000001110000011), 0b1110000011);
    // Translation  Bitmap Type              Port [2116h/17h]     VRAM Word-Address
    //  8bit rotate  4-color; 1 word/plane   aaaaaaaaYYYxxxxx --> aaaaaaaaxxxxxYYY
    p.write(0x2115, 1 << 2);
    assert_eq!(p.vram_addr_translate(0b0000000011100000), 0b111);
    assert_eq!(p.vram_addr_translate(0b0000000000011111), 0b11111000);
    assert_eq!(
        p.vram_addr_translate(0b1111111100000000),
        0b1111111100000000
    );
    //  9bit rotate  16-color; 2 words/plane aaaaaaaYYYxxxxxP --> aaaaaaaxxxxxPYYY
    p.write(0x2115, 2 << 2);
    assert_eq!(p.vram_addr_translate(0b0000000111000001), 0b1111);
    assert_eq!(p.vram_addr_translate(0b0000000000111110), 0b111110000);
    assert_eq!(
        p.vram_addr_translate(0b1111111000000000),
        0b1111111000000000
    );
    // 10bit rotate 256-color; 4 words/plane aaaaaaYYYxxxxxPP --> aaaaaaxxxxxPPYYY
    p.write(0x2115, 3 << 2);
    assert_eq!(p.vram_addr_translate(0b0000001110000011), 0b11111);
    assert_eq!(p.vram_addr_translate(0b0000000001111100), 0b1111100000);
    assert_eq!(
        p.vram_addr_translate(0b1111110000000000),
        0b1111110000000000
    );
}
