\m4_TLV_version 1d: tl-x.org
\SV
   // This code can be found in: https://github.com/stevehoover/RISC-V_MYTH_Workshop
   
   m4_include_lib(['https://raw.githubusercontent.com/stevehoover/RISC-V_MYTH_Workshop/c1719d5b338896577b79ee76c2f443ca2a76e14f/tlv_lib/risc-v_shell_lib.tlv'])

\SV
   m4_makerchip_module   // (Expanded in Nav-TLV pane.)
\TLV

   // /====================\
   // | Sum 1 to 9 Program |
   // \====================/
   //
   // Program for MYTH Workshop to test RV32I
   // Add 1,2,3,...,9 (in that order).
   //
   // Regs:
   //  r10 (a0): In: 0, Out: final sum
   //  r12 (a2): 10
   //  r13 (a3): 1..10
   //  r14 (a4): Sum
   // 
   // External to function:
   m4_asm(ADD, r10, r0, r0)             // Initialize r10 (a0) to 0.
   // Function:
   m4_asm(ADD, r14, r10, r0)            // Initialize sum register a4 with 0x0
   m4_asm(ADDI, r12, r10, 1010)         // Store count of 10 in register a2.
   m4_asm(ADD, r13, r10, r0)            // Initialize intermediate sum register a3 with 0
   // Loop:
   m4_asm(ADD, r14, r13, r14)           // Incremental addition
   m4_asm(ADDI, r13, r13, 1)            // Increment intermediate register by 1
   m4_asm(BLT, r13, r12, 1111111111000) // If a3 is less than a2, branch to label named <loop>
   m4_asm(ADD, r10, r14, r0)            // Store final result to register a0 so that it can be read by main program

   //Added by me - to check the Ld/Str functionality
   m4_asm(SW, r0, r10, 100)
   m4_asm(LW, r15, r0, 100)
   // Optional:
   // m4_asm(JAL, r7, 00000000000000000000) // Done. Jump to itself (infinite loop). (Up to 20-bit signed immediate plus implicit 0 bit (unlike JALR) provides byte address; last immediate bit should also be 0)
   m4_define_hier(['M4_IMEM'], M4_NUM_INSTRS)

   |cpu
      //Type 1 - Using Stalls (NOPS) to ovrcome Hazards
      @0
         $reset = *reset;
         /*
         $start = $reset ? 0 :
                  >>1$reset ? 1 : 
                  0; //default
         */
         //Generate a valid signal
         //Nullify this when including branches, jumps and ld/str
         /*
         $valid = $reset ? 0 :
                  $start ? 1 :
                  >>3$valid;
         */         
      // YOUR CODE HERE         
      //For invalid instructions (-since we won't have a valid instruction every cycle now
      //because of dependency hazards and NOPS
      // Use the generated $valid signal to : - 
      // a. PC should not change during a NOP
      // b. Reg Write into RF should not occur during NOP
      // c. Update inter-instructiob dependency aligments by waiting for 3 cycles before starting new instruction
         
  
      // ...
      //PC logic
         //$pc[31:0] = >>1$reset ? 0 : >>1$pc + 32'd4; // increment by 1 instruction - 4 bytes
         $pc[31:0] = >>1$reset ? 0 : 
                     >>3$taken_br ? >>3$br_tgt_pc : // changed >>3$valid_taken_br to $taken_vr
                     >>3$valid_load ? >>3$inc_pc : 
                     (>>3$is_jal && >>3$valid_jump) ? >>3$br_tgt_pc :
                     (>>3$is_jalr && >>3$valid_jump) ? >>3$jalr_tgt_pc :
                     >>1$inc_pc; //default
                     //Inserting NOPS in case of loads 
                    //Giving 3 cycle stall for load  - WHY ? More clarity needed
                     
         //Fetch logic // instruction memory is present
         
         $imem_rd_en = $reset ? 0 : 1;
         $imem_rd_addr[M4_IMEM_INDEX_CNT-1:0] = $pc[M4_IMEM_INDEX_CNT+1:2]; 
        
      @1
         $instr[31:0] = $imem_rd_en ? $imem_rd_data[31:0] : 0;
         $inc_pc[31:0] = $pc + 32'd4; //define $inc_pc to simplify $pc logic 
         //Decode Logic
         //A. Instruction type
         
         $is_b_instr = $instr[6:2] ==? 5'b11000;
         $is_u_instr = $instr[6:2] ==? 5'b0x101;
         $is_j_instr = $instr[6:2] ==? 5'b11011;
         $is_r_instr = $instr[6:2] ==? 5'b01011 ||
                       $instr[6:2] ==? 5'b0110x ||
                       $instr[6:2] ==? 5'b10100;
         $is_i_instr = $instr[6:2] ==? 5'b0000x ||
                       $instr[6:2] ==? 5'b001x0 ||
                       $instr[6:2] ==? 5'b11001;
                       //$instr[6:2] ==? 5'b11100;
         $is_s_instr = $instr[6:2] ==? 5'b0100x;
         
         //B. Immediate decode
         // R type doesn't have an immediate field - check
         $imm[31:0] = $is_b_instr ? { {20{$instr[31]}}, $instr[7], $instr[30:25], $instr[11:8], 1'b0} :
                      $is_u_instr ? { $instr[31], $instr[30:20], $instr[19:12], 12'b0} :
                      $is_j_instr ? { {12{$instr[31]}}, $instr[19:12], $instr[20], $instr[30:25], $instr[24:21], 1'b0} :
                      $is_i_instr ? { {21{$instr[31]}}, $instr[30:25], $instr[24:21], $instr[20]} :
                      $is_s_instr ? { {21{$instr[31]}}, $instr[30:25], $instr[11:8], $instr[7]} :
                      32'd0; //default - is this needed ?
         
         //C. Create valids for other fields depending on instr type
         $rd_valid = !$is_s_instr && !$is_b_instr;
         $funct3_valid = !$is_u_instr && !$is_j_instr;
         $rs1_valid = !$is_u_instr && !$is_j_instr;
         $rs2_valid = $is_r_instr || $is_s_instr || $is_b_instr;
         $funct7_valid = $is_r_instr;
         
         //D. Other fields decode
         $opcode[6:0] = $instr[6:0];
         ?$rd_valid
            $rd[4:0] = $instr[11:7];
         ?$funct3_valid
            $funct3[2:0] = $instr[14:12];
         ?$rs1_valid
            $rs1[4:0] = $instr[19:15];
         ?$rs2_valid
            $rs2[4:0] = $instr[24:20];
         ?$funct7_valid
            $funct7[6:0] = $instr[31:25];
         
         //E. Decode Individual instructions
         // Only a subset of RISCV spec - just what we need
         //A. Collect bits that are needed to specify an instruction
         $dec_bits[10:0] = {$funct7[5], $funct3, $opcode};
         //B. Now decode the istructions by matching patterns in Spec with $dec_bits
         //B.1 branch instructions
         $is_beq = $dec_bits ==? 11'bx_000_1100011;
         $is_bne = $dec_bits ==? 11'bx_001_1100011;
         $is_blt = $dec_bits ==? 11'bx_100_1100011;
         $is_bge = $dec_bits ==? 11'bx_101_1100011;
         $is_bltu = $dec_bits ==? 11'bx_110_1100011;
         $is_bgeu = $dec_bits ==? 11'bx_111_1100011;
         
         $is_jal = $dec_bits ==? 11'bx_xxx_1101111;
         $is_jalr = $dec_bits ==? 11'bx_000_1100111;
         
         $is_auipc = $dec_bits ==? 11'bx_xxx_0010111;
      
         //B.2 Arithmetic 
         $is_addi = $dec_bits ==? 11'bx_000_0010011;
         $is_add = $dec_bits == 11'b0_000_0110011;
         $is_sub = $dec_bits ==? 11'b1_000_0110011;
         
         //B.3 Logical
         $is_slti = $dec_bits ==? 11'bx_010_0010011;
         $is_xori = $dec_bits ==? 11'bx_100_0010011;
         $is_andi = $dec_bits ==? 11'bx_111_0010011;
         $is_slli = $dec_bits ==? 11'b0_001_0010011;
         $is_srli = $dec_bits ==? 11'b0_101_0010011;
         $is_srai = $dec_bits ==? 11'b1_101_0010011;
         $is_ori  = $dec_bits ==? 11'bx_110_0010011; 
         
         $is_sll = $dec_bits ==? 11'b0_001_0110011;
         $is_slt = $dec_bits ==? 11'b0_010_0110011;
         $is_xor = $dec_bits ==? 11'b0_100_0110011;
         $is_srl = $dec_bits ==? 11'b0_101_0110011;
         $is_sra = $dec_bits ==? 11'b1_101_0110011;
         $is_and = $dec_bits ==? 11'b0_111_0110011;
         $is_or  = $dec_bits ==? 11'b0_110_0110011;
         
         $is_sltiu = $dec_bits ==? 11'bx_011_0010011;
         $is_sltu  = $dec_bits ==? 11'b0_011_0110011;
         
         //B.4 Loads and Stores
         $is_lui = $dec_bits ==? 11'bx_xxx_0110111;
         $is_load = $dec_bits ==? 11'bx_xxx_0000011; // Concatenate all loads into 1 instruction for our purposes
         $is_store = $dec_bits ==? 11'bx_xxx_0100011; // Concatenate all stores into 1 instruction for our purposes
         
      @2
         //RF Read 
         $rf_rd_en1 = $rs1_valid;
         $rf_rd_en2 = $rs2_valid;
                      
                      
         $rf_rd_index1[4:0] = $rf_rd_en1 ? $rs1 : 0;
         $rf_rd_index2[4:0] = $rf_rd_en2 ? $rs2 : 0;
         
         //Rd read outputs
         /*
         $src1_value[31:0] = $rf_rd_en1 ? 
                             (>>1$rf_wr_en && (>>1$rd == $rs1)) ? >>1$result : 
                                                                $rf_rd_data1[31:0] : 
                             0; //default
         */
         $src1_value[31:0] = (>>1$rf_wr_en && (>>1$rd == $rs1)) ? >>1$result : 
                                                                $rf_rd_data1[31:0]; //modified
                             
         $src2_value[31:0] = (>>1$rf_wr_en && (>>1$rd == $rs2)) ? >>1$result : 
                                                                  $rf_rd_data2[31:0];
         
         
         //BRANCHES
         /*
         $beq = ($rf_rd_data1 == $rf_rd_data2);
         $bne = ($rf_rd_data1 != $rf_rd_data2);
         $bltu = ($rf_rd_data1 < $rf_rd_data2);
         $bgeu = ($rf_rd_data1 >= $rf_rd_data2);
         $blt = ($rf_rd_data1[31] != $rf_rd_data2[31]) ^ ($rf_rd_data1 < $rf_rd_data2);
         $bge = ($rf_rd_data1[31] != $rf_rd_data2[31]) ^ ($rf_rd_data1 >= $rf_rd_data2);
         */
         
         //Branch logic // change from $rf_rd_data1/2 to $src1/2_value
         $beq = ($src1_value == $src2_value);
         $bne = ($src1_value != $src2_value);
         $bltu = ($src1_value < $src2_value);
         $bgeu = ($src1_value >= $src2_value);
         $blt = ($src1_value[31] != $src2_value[31]) ^ ($src1_value < $src2_value);
         $bge = ($src1_value[31] != $src2_value[31]) ^ ($src1_value >= $src2_value);
         
         $br_tgt_pc[31:0] = $pc + $imm; 
         
         
      @3
         //ALU and output selection
         //first code partial results for sltu and sltiu instrs
         $sltu_rslt[31:0]  = $is_sltu  ? ($src1_value < $src2_value) : 0;
         $sltiu_rslt[31:0] = $is_sltiu ? ($src1_value < $imm) : 0; 
         
         //now code the final results based on intructions type (notice the diff for slt & slti)
         $result[31:0] = $is_addi ? $src1_value + $imm :
                         $is_add  ? $src1_value + $src2_value :
                         $is_andi ? $src1_value & $imm :
                         $is_ori  ? $src1_value | $imm :
                         $is_xori ? $src1_value ^ $imm :
                         $is_slli ? $src1_value << $imm[5:0] :
                         $is_srli ? $src1_value >> $imm[5:0] :
                         $is_and  ? $src1_value & $src2_value :
                         $is_or   ? $src1_value | $src2_value :
                         $is_xor  ? $src1_value ^ $src2_value :
                         $is_sub  ? $src1_value - $src2_value :
                         $is_sll  ? $src1_value << $src2_value[4:0] :
                         $is_srl  ? $src1_value >> $src2_value[4:0] : 
                         $is_sltu ? $sltu_rslt :
                         $is_sltiu ? $sltiu_rslt :
                         $is_lui ? {$imm[31:12], 12'd0} :
                         $is_auipc ? $pc + $imm :
                         $is_jal ? $pc + 32'd4 :
                         $is_jalr ? $pc + 32'd4 :
                         $is_sra ? { {32{$src1_value[31]}}, $src1_value} >> $src2_value[4:0] :
                         $is_srai ? { {32{$src1_value[31]}}, $src1_value} >> $imm[4:0] :
                         $is_slt ? ($src1_value[31] == $src2_value[31]) ? $sltu_rslt : {31'b0, $src1_value[31]} :
                         $is_slti ? ($src1_value[31] == $imm[31]) ? $sltiu_rslt : {31'b0, $src1_value[31]} :
                         //For loads and stores, compute the result(address)
                         ($is_load || $is_store) ? $src1_value + $imm : //this is same as addi and calculates the address of the load/store
                         32'b0; // default
                         
         //Change $valid logic to incorporate branches, loads and jumps
         //Nullify previously created $valid
         $valid = !(>>1$valid_taken_br || >>2$valid_taken_br || >>1$valid_load || >>2$valid_load);
         
         //Branch control 
         $taken_br = $is_beq ? $beq :
                     $is_bne ? $bne :
                     $is_bltu ? $bltu :
                     $is_bgeu ? $bgeu :
                     $is_blt ? $blt :
                     $is_bge ? $bge :
                     1'b0; //default
         
         $valid_taken_br = $taken_br && $valid; //added valid for NOPs
         
         //JALR PC calc - based on obtained $src1_value
         $jalr_tgt_pc[31:0] = $src1_value + $imm;
         //Check the flow of code - it seems sequential; could lead to potential problems           
         //Rf write
         // Dealing with RAW dependence through Forwarding
         //$rd_valid = $rd == 5'd0 ? 0 : 1;
         $rf_wr_en = (($rd_valid && $rd != 5'd0 && $valid) || >>2$valid_load); //$valid added for NOPS
                     
         //$rf_wr_index[4:0] = $rf_wr_en ? >>2$valid_load ? >>2$rd : $rd : 0; // previous incorrect logic
         $rf_wr_index[4:0] = $rf_wr_en ? !$valid ? >>2$rd : $rd : 0; // modified correct logic
         //modified to accomodate load instruction - defined the load address from when load instr occured (2 cycles earlier)
                  
         //$rf_wr_data[31:0] = $rf_wr_en ? >>2$result : 0; //my logic
         //$rf_wr_data[31:0] = $rf_wr_en ? $result : 0; //modified logic - this is correct if NO ld/str
         //New logic to accmodate load/store
         // Perform write of ld_data into RF after waiting for cycles after we go to mem to fetch ld_data
         // to wait for the ld_data to be returned from data cache 
         $rf_wr_data[31:0] = $rf_wr_en ? !$valid ? >>2$ld_data[31:0] : $result : 0;
         
         
         //Load control 
         $valid_load = $valid && $is_load;
         //$valid_store = $valid && $is_store; 
         $valid_store = $valid && $is_s_instr;
         
         $valid_jump = $valid && ($is_jal || $is_jalr);
      @4
         //Load/Store data memory interface connections //Come back again to gain clairy
         //Dmem is only either 1R or 1W per cycle
         //Read
         $dmem_rd_en = $valid_load;
                       
                       
         //$dmem_addr[3:0] = $valid_load ? $result[5:2] : 0; //The output of result[31:0] on a valid_load
                                                           //generates the address of the data memory to be read
                                                   // In this case, only the lower 4 bits are needed, since the memory has only 16 entries
         $dmem_addr[3:0] = $result[5:2]; // modified
         //Write 
         //$dmem_wr_en = $valid_store; 
         $dmem_wr_en = $valid && $is_s_instr; // modified
                      
         $dmem_wr_data[31:0] = $src2_value[31:0]; //Store gets value from second operand
                       
          
      @5
         $ld_data[31:0] = $dmem_rd_data; 
        
         
      // Note: Because of the magic we are using for visualisation, if visualisation is enabled below,
      //       be sure to avoid having unassigned signals (which you might be using for random inputs)
      //       other than those specifically expected in the labs. You'll get strange errors for these.
      
   //BOGUS USE to suppress warnings
   /*
   `BOGUS_USE($rd $rd_valid $rs1 $rs1_valid $rs2 $rs2_valid $instr $is_r_instr
              $is_i_instr $is_s_instr $is_b_instr $is_u_instr $is_j_instr
              $funct3 $funct7 $funct3_valid $funct7_valid $imm_valid $result
              $is_bge $is_bltu $is_bgeu $is_beq $is_bne $is_blt $is_addi $is_add
              $src1_value $src2_value $result
              $beq $bne $blt $bge $bltu $bgeu
              $taken_br $br_tgt_pc);
   */   
   
   //TB to check pass/fail by monitoring value in x10(r10) at the end of simulation
   // Assert these to end simulation (before Makerchip cycle limit).
   //*passed = *cyc_cnt > 40;
   *passed = |cpu/xreg[15]>>5$value == (1+2+3+4+5+6+7+8+9);
   *failed = 1'b0;
   
   // Macro instantiations for:
   //  o instruction memory
   //  o register file
   //  o data memory
   //  o CPU visualization
   |cpu
      m4+imem(@1)    // Args: (read stage) //Instruction mem in @1
      
      m4+rf(@2, @3)  // Args: (read stage, write stage) - if equal, no register bypass is required
      m4+dmem(@4)    // Args: (read/write stage)
   
   m4+cpu_viz(@4)    // For visualisation, argument should be at least equal to the last stage of CPU logic. @4 would work for all labs.
\SV
   endmodule
