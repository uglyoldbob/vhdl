library ieee ;
use ieee.std_logic_1164.all;
use ieee.std_logic_unsigned.all;
----------------------------------------------------
entity one_wire_master is
--clock period is in nanoseconds
generic (clock_period: integer);
port(
	dout: out std_logic;
	din: in std_logic;
	clock: in std_logic;
	reset: in std_logic
);
end entity one_wire_master;
----------------------------------------------------
architecture behavior of one_wire_master is
	--the numbers below are all nanoseconds, divided by clock period, to get actual clock cycle count
	constant max_delay : integer := 500000 / clock_period;
	constant write_1_low_time : integer := 8000 / clock_period;
	constant write_1_high_time : integer := 67000 / clock_period;
	constant write_0_low_time : integer := 57000 / clock_period;
	constant write_0_high_time : integer := 10000 / clock_period;
	constant read_low_time : integer := 8000 / clock_period;
	constant read_sample_delay_time : integer := 8000 / clock_period;
	constant read_high_time : integer := 59000 / clock_period;
	constant reset_low_time : integer := 480000 / clock_period;
	constant reset_pd_delay : integer := 70000 / clock_period;
	constant reset_inactive_time : integer := 410000 / clock_period;
	type state is (
		idle, 
		w1_active, w1_inactive,
		w0_active, w0_inactive,
		r_active, r_inactive1, r_sample, r_inactive2,
		reset_active, reset_inactive1, reset_sample_pd, reset_inactive2);
	type low_level_command is (
		idle, write0, write1, read_bit, reset_modules);
	signal low_level_state: state;
	signal low_level_timer: integer range 0 to max_delay + 1;
	signal calculated_dout: std_logic;
	signal last_read_bit: std_logic;
	signal devices_present: std_logic;
	
	--signals for driving the low level system
	signal low_level_idle : std_logic;
	signal low_level_request : low_level_command;
	
begin
	dout <= '1' when reset='0' else calculated_dout;
	low_level_request <= write1;
	process (clock)
	begin
		if rising_edge(clock) then
			case low_level_state is
				when idle => 
					low_level_idle <= '1';
					calculated_dout <= '1';
					low_level_timer <= 0;
					case low_level_request is
						when write0 => low_level_state <= w0_active;
						when write1 => low_level_state <= w1_active;
						when read_bit => low_level_state <= r_active;
						when reset_modules => low_level_state <= reset_active;
						when others => low_level_state <= idle;
					end case;
				when w1_active => 
					low_level_idle <= '0';
					calculated_dout <= '0';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= write_1_low_time) then
						low_level_state <= w1_inactive;
						low_level_timer <= 0;
					end if;
				when w1_inactive => 
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= write_1_high_time) then
						low_level_state <= idle;
						low_level_timer <= 0;
					end if;
				when w0_active => 
					low_level_idle <= '0';
					calculated_dout <= '0';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= write_0_low_time) then
						low_level_state <= w0_inactive;
						low_level_timer <= 0;
					end if;
				when w0_inactive => 
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= write_0_high_time) then
						low_level_state <= idle;
						low_level_timer <= 0;
					end if;
				when r_active =>
					low_level_idle <= '0';
					calculated_dout <= '0';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= read_low_time) then
						low_level_state <= r_inactive1;
						low_level_timer <= 0;
					end if;
				when r_inactive1 =>
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= read_sample_delay_time) then
						low_level_state <= r_sample;
						low_level_timer <= 0;
					end if;
				when r_sample =>
					low_level_idle <= '0';
					calculated_dout <= '1';
					last_read_bit <= din;
					low_level_state <= r_inactive2;
				when r_inactive2 =>
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= read_high_time) then
						low_level_state <= idle;
						low_level_timer <= 0;
					end if;
				when reset_active =>
					low_level_idle <= '0';
					calculated_dout <= '0';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= reset_low_time) then
						low_level_state <= reset_inactive1;
						low_level_timer <= 0;
					end if;
				when reset_inactive1 =>
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= reset_pd_delay) then
						low_level_state <= reset_sample_pd;
						low_level_timer <= 0;
					end if;
				when reset_sample_pd =>
					low_level_idle <= '0';
					calculated_dout <= '1';
					devices_present <= not din;
					low_level_state <= reset_inactive2;
				when reset_inactive2 =>
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= low_level_timer + 1;
					if (low_level_timer >= reset_inactive_time) then
						low_level_state <= idle;
						low_level_timer <= 0;
					end if;
				when others => 
					low_level_idle <= '0';
					calculated_dout <= '1';
					low_level_timer <= 0;
					low_level_state <= idle;
			end case;
		end if;
	end process;
end behavior;