pub struct AluResult {
    pub result: u16,
    pub c: bool,
    pub n: bool,
    pub z: bool,
    pub v: bool,
}

pub fn add16_dec(acc: u16, d: u16, carry: bool) -> AluResult {
    let acc: u32 = acc.into();
    let d: u32 = d.into();
    let mut c = if carry { 1 } else { 0 };
    let mut result: u32 = (acc & 0x000F) + (d & 0x000F) + c;
    if result > 0x0009 {
        result = result + (0x0006);
    }
    c = if result > 0x000F { 1 } else { 0 };
    result = (acc & 0x00F0) + (d & 0x00F0) + (c << 4) + (result & 0x000F);
    if result > 0x009F {
        result = result + (0x0060);
    }

    c = if result > 0x00FF { 1 } else { 0 };
    result = (acc & 0x0F00) + (d & 0x0F00) + (c << 8) + (result & 0x00FF);
    if result > 0x09FF {
        result = result + (0x0600);
    }
    c = if result > 0x0FFF { 1 } else { 0 };

    result = (acc & 0xF000) + (d & 0xF000) + (c << 12) + (result & 0x0FFF);
    let v = !(acc ^ d) & (acc ^ result) & 0x8000 != 0;
    if result > 0x9FFF {
        result = result + (0x6000);
    }

    AluResult {
        result: result as u16,
        z: result == 0,
        n: result & 0x8000 != 0,
        v,
        c: result > u16::MAX.into(),
    }
}

pub fn add16(acc: u16, d: u16, carry: bool) -> AluResult {
    let c = if carry { 1 } else { 0 };

    let result: u32 = acc as u32 + d as u32 + c;

    AluResult {
        result: result as u16,
        z: result == 0,
        n: result & 0x8000 != 0,
        v: !(acc ^ d) & (acc ^ result as u16) & 0x8000 != 0,
        c: result > u16::MAX.into(),
    }
}

pub fn add8(acc: u16, d: u16, carry: bool) -> AluResult {
    let c = if carry { 1 } else { 0 };

    let result: u16 = acc + d + c;

    AluResult {
        result: result & 0xFF,
        z: result as u8 == 0,
        n: result & 0x80 != 0,
        v: !(acc ^ d) & (acc ^ result) & 0x80 != 0,
        c: result > u8::MAX.into(),
    }
}

pub fn add8_dec(acc: u16, d: u16, carry: bool) -> AluResult {
    let mut c = if carry { 1 } else { 0 };
    let mut result: u16 = (acc & 0x000F) + (d & 0x000F) + c;
    if result > 0x0009 {
        result = result + (0x0006);
    }
    c = if result > 0x000F { 1 } else { 0 };
    result = (acc & 0x00F0) + (d & 0x00F0) + (c << 4) + (result & 0x000F);
    let v = !(acc ^ d) & (acc ^ result) & 0x80 != 0;
    if result > 0x009F {
        result = result + (0x0060);
    }

    AluResult {
        result: result & 0xFF,
        z: result as u8 == 0,
        n: result & 0x80 != 0,
        v,
        c: result > u8::MAX.into(),
    }
}
