--
--    This file is part of top_alphanumeric
--    Copyright (C) 2011  Julien Thevenon ( julien_thevenon at yahoo.fr )
--
--    This program is free software: you can redistribute it and/or modify
--    it under the terms of the GNU General Public License as published by
--    the Free Software Foundation, either version 3 of the License, or
--    (at your option) any later version.
--
--    This program is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU General Public License for more details.
--
--    You should have received a copy of the GNU General Public License
--    along with this program.  If not, see <http://www.gnu.org/licenses/>
--
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;

-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity image_controler is
  Port ( clk : in  STD_LOGIC;
         rst : in  STD_LOGIC;
         r : out  STD_LOGIC_VECTOR (5 downto 0);
         g : out  STD_LOGIC_VECTOR (5 downto 0);
         b : out  STD_LOGIC_VECTOR (5 downto 0);
         x : in  STD_LOGIC_VECTOR (9 downto 0);
         y : in  STD_LOGIC_VECTOR (8 downto 0);
         enable_in : in std_logic;
         hsync_in : in std_logic;
         vsync_in : in std_logic;
         write_enable : in std_logic;
         write_addr : in std_logic_vector(12 downto 0);
         char_code : in std_logic_vector(7 downto 0);
         color_code : in std_logic_vector(9 downto 0);
         enable_out : out std_logic;
         hsync_out : out std_logic;
         vsync_out : out std_logic);
end image_controler;

architecture Behavioral of image_controler is
------------ Begin Cut here for COMPONENT Declaration ------ COMP_TAG
component ram
	port (
	clka: IN std_logic;
	wea: IN std_logic_VECTOR(0 downto 0);
	addra: IN std_logic_VECTOR(12 downto 0);
	dina: IN std_logic_VECTOR(17 downto 0);
	clkb: IN std_logic;
	addrb: IN std_logic_VECTOR(12 downto 0);
	doutb: OUT std_logic_VECTOR(17 downto 0));
end component;

-- Synplicity black box declaration
attribute syn_black_box : boolean;
attribute syn_black_box of ram: component is true;

-- COMP_TAG_END ------ End COMPONENT Declaration ------------
component character_rom
	port (
	clka: IN std_logic;
	addra: IN std_logic_VECTOR(10 downto 0);
	douta: OUT std_logic_VECTOR(7 downto 0));
end component;

-- Synplicity black box declaration
attribute syn_black_box of character_rom: component is true;

-- COMP_TAG_END ------ End COMPONENT Declaration ------------

------------- Begin Cut here for COMPONENT Declaration ------ COMP_TAG
component color_rom
	port (
	clka: IN std_logic;
	addra: IN std_logic_VECTOR(9 downto 0);
	douta: OUT std_logic_VECTOR(17 downto 0));
end component;

-- Synplicity black box declaration
attribute syn_black_box of color_rom: component is true;

-- COMP_TAG_END ------ End COMPONENT Declaration ------------
-- Signals needed to write in memory
  signal ram_write_enable : std_logic_vector(0 downto 0);
  signal ram_data_in : std_logic_vector(17 downto 0);
  
-- Signals needed to read from ram
  signal ram_read_addr : std_logic_vector(12 downto 0) := (others => '0');
  signal ram_data_out : std_logic_vector(17 downto 0);

-- SIgnals to read from character rom
signal read_char_addr : std_logic_vector(10 downto 0);
signal read_char_data : std_logic_vector(7 downto 0);

-- SIgnals to read from character rom
signal read_color_addr : std_logic_vector(9 downto 0);
signal read_color_data : std_logic_vector(17 downto 0);

-- Signal defining column
signal column : std_logic_vector(6 downto 0) := (others => '0');
signal line : std_logic_vector(5 downto 0) := (others => '0');

signal pixel_state : std_logic ;
signal x_delayed : std_logic_vector(2 downto 0) ;
-- Constant defining the delay introduced by the image controler
constant controler_delay : natural := 2;
begin
------------- Begin Cut here for INSTANTIATION Template ----- INST_TAG
  my_ram : ram
    port map (
      clka => clk,
      wea => ram_write_enable,
      addra => write_addr,
      dina => ram_data_in,
      clkb => clk,
      addrb => ram_read_addr,
      doutb => ram_data_out);
-- INST_TAG_END ------ End INSTANTIATION Template ------------

------------- Begin Cut here for INSTANTIATION Template ----- INST_TAG
my_character_rom : character_rom
		port map (
			clka => clk,
			addra => read_char_addr,
			douta => read_char_data);
-- INST_TAG_END ------ End INSTANTIATION Template ------------

------------- Begin Cut here for INSTANTIATION Template ----- INST_TAG
my_color_rom : color_rom
		port map (
			clka => clk,
			addra => read_color_addr,
			douta => read_color_data);
-- INST_TAG_END ------ End INSTANTIATION Template ------------

  -- Block to introduce delays need to have control signals
  -- synchronous with colour signals
  hsync_delayer : entity work.bit_delay
    generic map (
      size => controler_delay)
    port map (
      clk => clk,
      rst => rst,
      input => hsync_in,
      output => hsync_out);
  
  vsync_delayer : entity work.bit_delay 
    generic map (
      size => controler_delay)
    port map (
      clk => clk,
      rst => rst,
      input => vsync_in,
      output => vsync_out);
  
  enable_delayer : entity work.bit_delay 
    generic map (
      size => controler_delay)
    port map (
      clk => clk,
      rst => rst,
      input => enable_in,
      output => enable_out);
  
  x_delayer : entity work.delay_register
  generic map(
    width => 3,
    delay => controler_delay)
  port  map(
    clk    => clk,
    rst => rst,
    input => x(2 downto 0),             -- input
    output  => x_delayed);            -- output

  -- Process controling adress of read port
--TO DELETE  process(clk,rst)
--TO DELETE  begin
--TO DELETE    if rst = '1' then
--TO DELETE      read_addr <= (others => '0');
--TO DELETE    elsif rising_edge(clk) and enable_in = '1' and x(2 downto 0) = "111" and y(2 downto 0) = "111" then
--TO DELETE      if unsigned(read_addr) /= 4799 then
--TO DELETE        read_addr <= std_logic_vector(unsigned(read_addr) + 1);
--TO DELETE      else 
--TO DELETE        read_addr <= (others => '0');
--TO DELETE      end if;
--TO DELETE    end if;
--TO DELETE  end process;
  
  -- Extracting coordinate from pixel in term of character array
  column <= x(9 downto 3);
  line <= y(8 downto 3);
  -- Rebulding corresponding memory addr
  ram_read_addr <= line & column;
  -- Getting char and color code
   read_color_addr <= ram_data_out(17 downto 8);
   read_char_addr <= ram_data_out(7 downto 0) & y(2 downto 0);

--   with x(2 downto 0) select
   with x_delayed select
       pixel_state <= read_char_data(7) when "000",
      read_char_data(6) when "001",
      read_char_data(5) when "010",
      read_char_data(4) when "011",
      read_char_data(3) when "100",
      read_char_data(2) when "101",
      read_char_data(1) when "110",
      read_char_data(0) when "111",
      '0' when others;
     
  -- Getting color value and pixel value
  r <= read_color_data(17 downto 12) when pixel_state = '1' else (others => '0');
  g <= read_color_data(11 downto 6) when pixel_state = '1' else (others => '0');
  b <= read_color_data(5 downto 0) when pixel_state = '1' else (others => '0');

  ram_write_enable <= (others => write_enable);
  ram_data_in <= color_code & char_code;
 
 end Behavioral;

