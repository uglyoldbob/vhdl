library ieee; 
use ieee.std_logic_1164.all;

entity motherboard is
	port (
		test: out std_logic);
end entity motherboard;

architecture behavior of motherboard is
	component sram	--2mbit, 10ns, 3.3v, $3.65 (1.74 udollars per bit)
		port(
			addr: in atd_logic_vector(18 downto 0);
			dq: inout std_logic_vector(7 downto 0);
			ce_n: in std_logic;
			oe_n: in std_logic;
			we_n: in std_logic);
	component sdram	--128mbit, 166mhz, 3.3v, $3.17 (23.62 nano dollars per bit)
		port(
			ck: in std_logic;
			cke: in std_logic;
			cs_n: in std_logic;
			ras_n: in std_logic;
			cas_n: in std_logic;
			we_n: in std_logic;
			address: in std_logic_vector(11 downto 0);
			ba: in std_logic_vector(1 downto 0);
			dq: inout std_logic_vector(7 downto 0);
			dqml: in std_logic;
			dqmh: in std_logic);
	end component;
	component sdram_slot_168_pins
		port(
			data: inout std_logic_vector(63 downto 0);
			cb: inout std_logic_vector(15 downto 0);
			we: in std_logic;
			dqmb: in std_logic_vector(7 downto 0);
			cs: in std_logic_vector(3 downto 0);
			addr: in std_logic_vector(13 downto 0);
			cas: in std_logic;
			ras: in std_logic;
			ba: in std_logic_vector(1 downto 0);
			clock: in std_logic_vector(3 downto 0);
			clock_enable: in std_logic_vector(1 downto 0);
			sda: inout std_logic;
			scl: in std_logic;
			sa: in std_logic_vector(2 downto 0);
			--46 unused, power, ground
			);
	component ddr3_ram	--1gbit, 800mhz, 1.5v, $6.02 (5.61 nano dollars per bit)
		port(
			ck: in std_logic;
			ck_n: in std_logic;
			cke: in std_logic;
			reset_n: in std_logic;
			cs_n: in std_logic;
			ras_n: in std_logic;
			cas_n: in std_logic;
			we_n: in std_logic;
			a10_ap: in std_logic;
			a12_bc_n: in std_logic;
			address: in std_logic_vector(9 downto 0);
			addr11: in std_logic;
			addr13: in std_logic;
			ba: in std_logic_vector(2 downto 0);
			vssq: in std_logic;
			dqs: inout std_logic;
			dqs_n: inout std_logic;
			tdqs: inout std_logic;
			tdqs_n: inout std_logic;
			dq: inout std_logic_vector(7 downto 0);
			odt: in std_logic;
			dm: in std_logic);
	end component;
	component ddr3_slot_240pin
		port(
			addr: in std_logic_vector(15 downto 0);	--addr(10) and addr(13) have special purpose
			ba: in std_logic_vector(2 downto 0);
			dq: inout std_logic_vector(63 downto 0);
			cb: inout std_logic_vector(7 downto 0);
			clock: in std_logic_vector(3 downto 0);
			clock_n: in std_logic_vector(3 downto 0);
			clock_enable: in std_logic_vector(1 downto 0);
			cs: in std_logic_vector(3 downto 0);
			ras: in std_logic;
			cas: in std_logic;
			we: in std_logic;
			dm: in std_logic_vector(8 downto 0);
			dqs: in std_logic_vector(8 downto 0);
			odt: in std_logic_vector(1 downto 0);
			par: in std_logic;
			err: out std_logic;
			scl: in std_logic;
			sda: inout std_logic;
			sa: in std_logic(1 downto 0);
			reset: in std_logic;
			test: inout std_logic);
	end component;
	--80gb hard drive, $19.99, (0.03 nano dollars per bit)
	component sata
		port(
			txa: out std_logic;
			txb: out std_logic;
			rxa: in std_logic;
			rxb: in std_logic);
	end component;
begin
	test <= '0';
end behavior;

--potential problems
	--row hammer
	--ram data remanence
	