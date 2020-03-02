-------------------------------------------------------------------------------------------------------------
-- Design unit: R8_uC
-- Description: Instantiation of R8 processor with an I/O Port, meant for synthisys on Nexys 3 board
-- Author: Carlos Gewehr and Emilio Ferreira (cggewehr@gmail.com, emilio.ferreira@ecomp.ufsm.br)
------------------------------------------------------------------------------------------------------------

library IEEE;
use IEEE.std_logic_1164.all;
--use IEEE.std_logic_unsigned.all;
use IEEE.numeric_std.all;

entity R8_uC is
    generic (
        RAM_IMAGE     : string;
        ROM_IMAGE     : string;
        ADDR_PORT     : std_logic_vector(3 downto 0);
        ADDR_PIC      : std_logic_vector(3 downto 0);
        ADDR_UART_TX  : std_logic_vector(3 downto 0);
        ADDR_UART_RX  : std_logic_vector(3 downto 0);
        ADDR_TIMER    : std_logic_vector(3 downto 0)
    );
	port (
		clk           : in std_logic;                        -- 50MHz from DCM
		rst           : in std_logic;                        -- Synchronous reset
        port_io       : inout std_logic_vector(15 downto 0); -- Communication port (IN/OUT, set by software)
        uart_tx       : out std_logic;                       -- Serial transmitter
        uart_rx       : in std_logic;                        -- Serial receiver
        prog_mode     : in std_logic                         -- 0 for execution, 1 for programming (loading RAM)
	);
end R8_uC;

architecture behavioral of R8_uC is
   
    signal ce, rw                                   : std_logic;                      -- I/O operation being carried out
    signal rw_MEM, clk_MEM, en_RAM, en_ROM, en_PORT : std_logic;                      -- Enables for I/O operation targets

    signal data_PORT, data_ROM_out, data_RAM_out    : std_logic_vector(15 downto 0);  -- Peripherals/Memory interface
    signal data_r8_in, data_r8_out, address         : std_logic_vector(15 downto 0);  -- Processor interface

    signal rst_R8                                   : std_logic;
    signal prog_mode_change_flag                    : std_logic;

    alias address_PORT                              : std_logic_vector(1 downto 0) is address(1 downto 0);   -- Address for I/O Port registers ( Data, Config, Enable or InterruptEnable)
    alias mem_address                               : std_logic_vector(14 downto 0)is address(14 downto 0);  -- Memory (RAM and ROM) address space (15th bit is reserved for I/O operations (I/O port, PIC, UART)
    alias ID_PERIFERICO                             : std_logic_vector(3 downto 0) is address(7 downto  4);  -- Peripheral ID
    alias REG_PERIFERICO                            : std_logic_vector(3 downto 0) is address(3 downto  0);  -- Peripheral Address
    alias ENABLE_PERIFERICO                         : std_logic is address(15); 							 -- Perfipheral Enable (I/O operation to be carried out on peripheral)
	
    -- Tristate for bidirectional bus between processor and I/O port
	signal TRISTATE_PORT_EN   : std_logic;
    signal TRISTATE_PIC_EN    : std_logic;
	
	-- Interruption Interface
	signal irq_R8             : std_logic;
	signal irq_PORT           : std_logic_vector(15 downto 0);
	
	-- Pic signals
	signal en_pic             : std_logic;
	signal intr_pic           : std_logic;
	signal irq_pic            : std_logic_vector(7 downto 0);
    signal data_PIC           : std_logic_vector(7 downto 0);

    -- UART TX signals
    signal ce_UART_TX         : std_logic;
    signal data_av_UART_TX    : std_logic;
    signal data_in_UART_TX    : std_logic_vector(15 downto 0);
    signal data_out_UART_TX   : std_logic_vector(15 downto 0);
    signal ready_UART_TX      : std_logic;

    -- UART RX signal
    signal ce_UART_RX         : std_logic;
    signal data_av_UART_RX    : std_logic;
    signal data_in_UART_RX    : std_logic_vector(15 downto 0);
    signal data_out_UART_RX   : std_logic_vector(15 downto 0);
	 
	 -- Timer signal
	 signal time_out_TIMER     : std_logic;
	 signal ce_timer           : std_logic;
	 signal tristate_timer_en  : std_logic;
	 signal data_timer         : std_logic_vector(15 downto 0);

begin
		
    -- Processor signals
    data_r8_in <= data_PORT               when ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_PORT    else
                  ("00000000" & data_PIC) when ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_PIC     else
                  data_out_UART_TX        when ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_UART_TX else
                  data_out_UART_RX        when ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_UART_RX else
                  data_TIMER              when ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_TIMER   else
                  data_ROM_out            when ENABLE_PERIFERICO = '0' and prog_mode = '1'              else
                  data_RAM_out;
                      
    rst_R8 <= '1' when rst = '1' or prog_mode_change_flag = '1' else '0';

    R8_RESET_HANDLER: process(clk, rst)
        variable prog_mode_current, prog_mode_prev : std_logic;               
    begin
        
        if rst = '1' then
            
            prog_mode_current := '0';
            prog_mode_prev := '0';

        elsif rising_edge(clk) then
            
            prog_mode_prev := prog_mode_current;
            prog_mode_current := prog_mode; -- From entity interface
            
            if prog_mode_current /= prog_mode_prev then -- Switch flipped, reset processor (start transfering program to RAM through UART or executing code in RAM)
                prog_mode_change_flag <= '1';
            else
                prog_mode_change_flag <= '0';
            end if;
                
        end if;
    end process;
                      
    -- Processor
    R8Processor: entity work.R8 
        port map(
            clk       => clk,
            rst       => rst_R8,
			   irq       => intr_PIC,
            prog_mode => prog_mode,
            address   => address,
            data_out  => data_r8_out,
            data_in   => data_r8_in,
            ce        => ce,
            rw        => rw                 -- Write : 0, Read : 1
        );
		
    -- RAM signals
    clk_MEM <= not clk;   -- Makes memory sensitive to falling edge
    rw_MEM  <= not rw;    -- Writes when 1, Reads when 0
    en_RAM  <= '1' when (ce = '1' and ENABLE_PERIFERICO = '0' and (prog_mode = '0' or (prog_mode = '1' and rw_MEM = '1'))) else '0'; -- Enabled when programming mode is off, or when programming mode is on and storing (stores on RAM)    
    
    -- RAM (software programmable)
    RAM: entity work.Memory 
        generic map(
            DATA_WIDTH => 16,
            ADDR_WIDTH => 15,
            IMAGE => RAM_IMAGE, -- Assembly code (must be in same directory)
            SIZE => 32768
        )
        port map(
            clk => clk_MEM,
            wr  => rw_MEM,
            en  => en_RAM,
            address  => mem_address, 
            data_in  => data_r8_out,
            data_out => data_RAM_out
        );

    -- ROM signals
    en_ROM <= '1' when (ce = '1' and ENABLE_PERIFERICO = '0' and prog_mode = '1' and rw_MEM = '0') else '0'; -- Enabled when programming mode is on and reading     

    -- ROM (contains RAM programming routine)
    ROM: entity work.Memory 
        generic map(
            DATA_WIDTH => 16,
            ADDR_WIDTH => 15,
            IMAGE => ROM_IMAGE, -- Assembly code (must be in same directory)
            SIZE => 200
        )
        port map(
            clk => clk_MEM,
            wr  => rw_MEM,
            en  => en_ROM,
            address  => mem_address, 
            data_in  => data_r8_out,
            data_out => data_ROM_out    
        );

    -- Port signals
    en_PORT <= '1' when (ce = '1' and ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_PORT) else '0';   
        
    -- Tristate between I/O port and processor
	data_PORT <= data_r8_out when TRISTATE_PORT_EN = '1' else (others=>'Z');
	TRISTATE_PORT_EN <= '1' when rw = '0' and ID_PERIFERICO = ADDR_PORT and ENABLE_PERIFERICO = '1' else '0';  -- Enables when writes

    -- I/O port
    IO_Port: entity work.BidirectionalPort
        generic map(
			DATA_WIDTH          => 16, -- Port width in bits
			PORT_DATA_ADDR      => "00",    
			PORT_CONFIG_ADDR    => "01",     
			PORT_ENABLE_ADDR    => "10",
			PORT_IRQ_ADDR       => "11"
        )
        port map(
            clk => clk, 
            rst => rst,

            -- Processor Interface
            data => data_PORT,
            address => address_PORT,
            rw => rw_MEM,              -- 0: read; 1: write
            ce => en_PORT,
		    irq => irq_PORT,           -- To PIC
				
            -- External interface
			port_io => port_io         -- To Peripheral
        );

    -- PIC signals
    en_PIC <= '1' when (ce = '1' and ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_PIC) else '0';
	 
    irq_PIC(7) <= irq_PORT(15);
    irq_PIC(6) <= irq_PORT(14);
    irq_PIC(5) <= irq_PORT(13);
    irq_PIC(4) <= irq_PORT(12);
    irq_PIC(3) <= '0';
    irq_PIC(2) <= '0';
    irq_PIC(1) <= data_av_UART_RX;
    irq_PIC(0) <= time_out_TIMER;

    -- Tristate between PIC and Processor
    data_PIC <= data_r8_out(7 downto 0) when TRISTATE_PIC_EN = '1' else (others=>'Z');
    TRISTATE_PIC_EN <= '1' when rw = '0' and ID_PERIFERICO = ADDR_PIC and ENABLE_PERIFERICO = '1' else '0';  -- Enables when writes

    -- Peripheral Interrupt Controller:
    PIC: entity work.InterruptController
        generic map(
            IRQ_ID_ADDR    => "0000",
            INT_ACK_ADDR   => "0001",
            MASK_ADDR      => "0010",
            IRQ_REG_ADDR   => "0011"
        )   
        port map(
            clk       => clk,
            rst       => rst,
            data      => data_PIC,
            address   => REG_PERIFERICO,
            rw        => rw_MEM,         -- 0: read; 1: write
            ce        => en_PIC,
            intr      => intr_PIC,       -- To processor
            irq       => irq_PIC         -- From port
        );

    -- UART TX signals
    ce_UART_TX <= '1' when (ce = '1' and ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_UART_TX) else '0';
    data_in_UART_TX <= data_r8_out;                                                      --(TX_DATA_ADDR)
    data_av_UART_TX <= '1' when rw = '0' and ID_PERIFERICO = ADDR_UART_TX and REG_PERIFERICO = "0000" and ENABLE_PERIFERICO = '1' else '0';
                                                                                         
    -- Serial transmitter
    TX: entity work.UART_TX
        generic map(
            TX_DATA_ADDR        => "0000",
            RATE_FREQ_BAUD_ADDR => "0001",
            READY_ADDR          => "0010"
        )
        port map (
            clk      => clk,
            rst      => rst,
            ce       => ce_UART_TX,
            rw       => rw_MEM,
            tx       => uart_tx,
            address  => REG_PERIFERICO,
            data_in  => data_in_UART_TX,
            data_out => data_out_UART_TX,
            data_av  => data_av_UART_TX,
            ready    => ready_UART_TX
        );

    -- UART RX signals
    data_in_UART_RX <= data_r8_out;
    ce_UART_RX <= '1' when (ce = '1' and ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_UART_RX) else '0';

    -- Serial receiver
    RX: entity work.UART_RX
        generic map(
            RX_DATA_ADDR        => "0000",
            RATE_FREQ_BAUD_ADDR => "0001"
        )
        port map(
            clk      => clk,
            rst      => rst,
            ce       => ce_UART_RX,
            rw       => rw_MEM,
            rx       => uart_rx,
            address  => REG_PERIFERICO,
            data_in  => data_in_UART_RX,
            data_out => data_out_UART_RX,
            data_av  => data_av_UART_RX
        );

    -- Timer signals
    ce_TIMER <= '1' when (ce = '1' and ENABLE_PERIFERICO = '1' and ID_PERIFERICO = ADDR_TIMER) else '0';
    data_TIMER <= data_r8_out when TRISTATE_TIMER_EN = '1' else (others=>'Z');
    TRISTATE_TIMER_EN  <= '1' when rw = '0' and ID_PERIFERICO = ADDR_TIMER and ENABLE_PERIFERICO = '1' else '0';

    -- Programmable timer
    TIMER: entity work.timer
        generic map(
            DATA_WIDTH => 16,
            COUNTER_ADDR => "0000"
        )
        port map(
            clk      => clk,
            rst      => rst,
            ce       => ce_TIMER,
            rw       => rw_MEM,
            address  => REG_PERIFERICO,
            data     => data_TIMER,
            time_out => time_out_TIMER
        );

end behavioral;