library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity lfsr32 is
	port (
	clock: in std_logic;
	data: out std_logic_vector(7 downto 0)
	);
end lfsr32;

architecture behavior of lfsr32 is
	signal shift_reg: std_logic_vector(32 downto 1) := x"00000001";
	signal lock_prevent: std_logic := '0';
	signal bit_calc: std_logic := '1';
begin
	process (clock)
	begin
		if clock='1' and clock'event then
			shift_reg <= shift_reg(31 downto 1) & (bit_calc xor lock_prevent);
		end if;
	end process;
	data <= shift_reg(8 downto 1);
	bit_calc <= shift_reg(32) xor shift_reg(22) xor shift_reg(2) xor shift_reg(1);
end behavior;

library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;

entity lfsr is
	generic(
	data_width: integer := 8;
	shift_size: integer := 32);
	port (
	clock: in std_logic;
	data: out std_logic_vector(data_width-1 downto 0);
	poly: in std_logic_vector(shift_size-1 downto 0)
	);
end lfsr;

architecture behavior of lfsr is
	signal shift_reg: std_logic_vector(shift_size-1 downto 0) := (others => '1');
	signal zero_prevent: std_logic;
	signal full_prevent: std_logic;
	signal calc: std_logic_vector(shift_size-1 downto 0) := (others => '0');
	signal bit_calc: std_logic := '1';
begin
	process (clock)
	begin
		if clock='1' and clock'event then
			shift_reg <= shift_reg(shift_size-1 downto 1) & (bit_calc or zero_prevent) and not full_prevent;
		end if;
	end process;
	bit_calc <= xor calc;
	zero_prevent <= nor shift_reg;
	full_prevent <= and shift_reg;
	calc <= shift_reg and poly;
	data <= shift_reg(data_width-1 downto 0);
end behavior;