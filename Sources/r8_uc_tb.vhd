library IEEE;
use IEEE.std_logic_1164.all;
use IEEE.numeric_std.all;

entity r8_uC_tb is
end r8_uC_tb;

architecture behavioral of r8_uC_tb is

    signal clk : std_logic := '0';
    signal clk_tx : std_logic := '0';
    signal rst : std_logic;
    signal port_io : std_logic_vector(15 downto 0);
    signal uart_rx_data_out : std_logic_vector(7 downto 0);
    signal uart_rx_data_av : std_logic;
	signal uart_tx : std_logic;
	signal uart_rx : std_logic;
    signal prog_mode : std_logic;
	
	signal ce_tx : std_logic;
	signal rw_tx : std_logic;
	signal addr_tx : std_logic_vector(3 downto 0);
	signal data_in_tx : std_logic_vector(15 downto 0);
	signal data_out_tx : std_logic_vector(15 downto 0);
	signal data_av_tx : std_logic;
	signal ready_tx : std_logic;
	
	signal data_sim : integer;
	signal av_sim : std_logic;
	signal count : integer;
	
begin
    R8: entity work.R8_uC_TOPLVL
        port map(
            clock => clk,
            reset => rst,
            port_io_uC => port_io,
            uart_tx => uart_tx,
            uart_rx => uart_rx,
            prog_mode => prog_mode
        );

    clk <= not clk after 5 ns; -- 100 MHz
	 clk_tx <= not clk_tx after 10 ns;
    rst <= '1', '0' after 15 ns;
	 port_io <= "ZZZZZZZZZZZZZZZZ";
    prog_mode <= '0';--, '0' after 11 us;

--
--    -- UART RX
--    UART_RX: entity work.UART_RX
--        generic map(
--            RATE_FREQ_BAUD => 8
--        )
--
--        port map(
--            clk => clk,
--            rst => rst,
--            rx => uart_tx,
--            data_out => uart_rx_data_out,
--            data_av => uart_rx_data_av
--        );

	-- UART TX Signals
	ce_tx <= '1';
	rw_tx <= '1';
	addr_tx <= "0001", "0000" after 50 ns;
	data_in_tx <= std_logic_vector(to_unsigned(count, data_in_tx'length));
	data_av_tx <= av_sim;

	TX: entity work.UART_TX
		generic map(
			TX_DATA_ADDR => "0000",
			RATE_FREQ_BAUD_ADDR => "0001",
			READY_ADDR => "0010"
		)
		port map(
			clk => clk_tx, 
			rst => rst,
			ce => ce_tx,
			rw => rw_tx,
			tx => uart_rx,
			address => addr_tx,
			data_in => data_in_tx,
			data_out => data_out_tx,
			data_av => data_av_tx,
			ready => ready_tx
		);
			
	SIM: process begin
	
		count <= 868;
		av_sim <= '0';
		
		wait for 50 ns;
		
		--av_sim <= '0';
		
		wait for 7 ms;

	   count <= 54; -- ASCII 6
	   av_sim <= '1';
		  
	   wait for 50 ns;
		  
      av_sim <= '0';
		  
  	   wait for 500 us;
		  
		count <= 10;
		av_sim <= '1';
		  
		wait for 50 ns;
		  
		av_sim <= '0';
			
	    --end loop;
	end process;
	
end behavioral;
