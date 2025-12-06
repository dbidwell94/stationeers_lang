use crc32fast::hash as crc32_hash;
/// This function takes an input which is meant to be hashed via the CRC32 algorithm, but it then
/// converts the generated UNSIGNED number into it's SIGNED counterpart.
pub fn crc_hash_signed(input: &str) -> i128 {
    let hash = crc32_hash(input.as_bytes());

    // in stationeers, crc32 is a SIGNED int.
    let hash_value_i32 = i32::from_le_bytes(hash.to_le_bytes());

    hash_value_i32 as i128
}
