#ifndef _GNU_SOURCE
	#define _GNU_SOURCE             /* See feature_test_macros(7) */
#endif
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/syscall.h>
#define gettid() syscall(SYS_gettid)
#include "libraries/ght.h"
#include "libraries/ghe.h"
#include "libraries/gc_top.h"
#include <time.h>
#include "libraries/spin_lock.h"
#include "libraries/encoding.h"
#include "libraries/sbi_ecall.h"
#include "libraries/sbi_ecall_interface.h"

#define totalcsrperf 84

uint64_t csr_read_s[totalcsrperf];
uint64_t csr_read_e[totalcsrperf];

// struct sbiret sbi_ecall(int ext, int fid, unsigned long arg0,
// 	unsigned long arg1, unsigned long arg2,
// 	unsigned long arg3, unsigned long arg4,
// 	unsigned long arg5)
// {
// struct sbiret ret;

// register uintptr_t a0 asm ("a0") = (uintptr_t)(arg0);
// register uintptr_t a1 asm ("a1") = (uintptr_t)(arg1);
// register uintptr_t a2 asm ("a2") = (uintptr_t)(arg2);
// register uintptr_t a3 asm ("a3") = (uintptr_t)(arg3);
// register uintptr_t a4 asm ("a4") = (uintptr_t)(arg4);
// register uintptr_t a5 asm ("a5") = (uintptr_t)(arg5);
// register uintptr_t a6 asm ("a6") = (uintptr_t)(fid);
// register uintptr_t a7 asm ("a7") = (uintptr_t)(ext);
// asm volatile ("ecall"
// 	  : "+r" (a0), "+r" (a1)
// 	  : "r" (a2), "r" (a3), "r" (a4), "r" (a5), "r" (a6), "r" (a7)
// 	  : "memory");
// ret.error = a0;
// ret.value = a1;

// return ret;
// }
// EXPORT_SYMBOL(sbi_ecall);

// #define NUM_EVENTS 21

// struct sbiret ret;
// uint32_t event_idx;
// int ctr_indices[NUM_EVENTS]; // 保存3个计数器的索引

// struct pmu_event {
//     uint32_t cidx_base;    // 计数器起始索引（如3）
//     uint64_t select_val;   // 事件编码（如0x0200）
//     uint64_t select_mask;  // 事件掩码（如0xFFFF）
// } events[NUM_EVENTS] = {
//     {3, 0x0200, 0x03FF},           // 事件0: mhpmevent3
//     {4, 0x0400, 0x05FF},           // 事件1: mhpmevent4
//     {5, 0x0800, 0x07FF},           // 事件2: mhpmevent5
// 	{6, 0x1000, 0x1FFF},           // 事件3: mhpmevent6
//     {7, 0x2000, 0x3FFF},           // 事件4: mhpmevent7
//     {8, 0x4000, 0x5FFF},           // 事件5: mhpmevent8
// 	{9, 0x8000, 0x7FFF},           // 事件6: mhpmevent9
//     {10, 0x10000, 0x1FFFF},        // 事件7: mhpmevent10
//     {11, 0x20000, 0x3FFFF},        // 事件8: mhpmevent11
// 	{12, 0x40000, 0x5FFFF},        // 事件9: mhpmevent12
//     {13, 0x80000, 0x7FFFF},        // 事件10: mhpmevent13
//     {14, 0x100000, 0x1FFFFF},      // 事件11: mhpmevent14
// 	{15, 0x200000, 0x3FFFFF},      // 事件12: mhpmevent15
//     {16, 0x400000, 0x5FFFFF},      // 事件13: mhpmevent16
//     {17, 0x800000, 0x7FFFFF},      // 事件14: mhpmevent17
// 	{18, 0x1000000, 0x1FFFFFF},    // 事件15: mhpmevent18
//     {19, 0x2000000, 0x3FFFFFF},    // 事件16: mhpmevent19
//     {20, 0x4000000, 0x5FFFFFF},    // 事件17: mhpmevent20
// 	{21, 0x8000000, 0x7FFFFFF},    // 事件18: mhpmevent21
//     {22, 0x10000000, 0x1FFFFFFF},  // 事件19: mhpmevent22
//     {23, 0x20000000, 0x3FFFFFFF},  // 事件20: mhpmevent23
// };

// size_t total_cycle_s;  
// size_t total_commit_s; 
// size_t total_csr_s;  
// size_t csrw_s; 
// size_t mstatus_s;  
// size_t misa_s;  
// size_t medeleg_s;      
// size_t mideleg_s;
// size_t mie_s;         
// size_t mtvec_s;     
// size_t mcounen_s;
// size_t mscratch_s;
// size_t mepc_s;
// size_t mtval_s;
// size_t mip_s;
// size_t mtval2_s;
// size_t menvcfg_s;
// size_t mseccfg_s;
// size_t pmpcfg0_s;
// size_t pmpaddr0_s;
// size_t mcountinh_s;
// size_t fflags_s;

// size_t total_cycle_e;  
// size_t total_commit_e; 
// size_t total_csr_e;  
// size_t csrw_e; 
// size_t mstatus_e;  
// size_t misa_e;  
// size_t medeleg_e;      
// size_t mideleg_e;
// size_t mie_e;         
// size_t mtvec_e;     
// size_t mcounen_e;
// size_t mscratch_e;
// size_t mepc_e;
// size_t mtval_e;
// size_t mip_e;
// size_t mtval2_e;
// size_t menvcfg_e;
// size_t mseccfg_e;
// size_t pmpcfg0_e;
// size_t pmpaddr0_e;
// size_t mcountinh_e;
// size_t fflags_e;

void perfstart(){  
	for(int i = 0; i < totalcsrperf; i++){
		csr_read_s[i] = ghe_csr_perf_read(i);
	  }
	printf("Perf Test begin \n");
	// // 批量配置事件
	// for (int i = 0; i < NUM_EVENTS; i++) {
    //     uint32_t event_idx = (0x2 << 16) | 0; // SBI_PMU_EVENT_TYPE_HW_RAW
    //     ret = sbi_ecall(
    //         SBI_EXT_PMU,
    //         SBI_EXT_PMU_COUNTER_CFG_MATCH,
    //         events[i].cidx_base,   // a0: cidx_base
    //         0x1,                   // a1: cidx_mask（单个计数器）
    //         SBI_PMU_CFG_FLAG_AUTO_START, // a2: 自动启动
    //         event_idx,             // a3: event_idx
    //         events[i].select_val,  // a4: event_data（选择值）
    //         events[i].select_mask // a5: select_mask
    //     );
    //     if (ret.error) {
    //         // 错误处理（如打印日志）
    //         printf("Event %d (mhpmevent%d) config failed: %d\n", 
    //                i, events[i].cidx_base, ret.error);
    //     }
    // }

	// total_cycle_s     = read_csr(cycle);
	// total_commit_s    = read_csr(hpmcounter3);
	// total_csr_s       = read_csr(hpmcounter4);
	// csrw_s            = read_csr(hpmcounter5);
	// mstatus_s         = read_csr(hpmcounter6); 
	// misa_s            = read_csr(hpmcounter7); 
	// medeleg_s         = read_csr(hpmcounter8); 
	// mideleg_s         = read_csr(hpmcounter9); 
	// mie_s             = read_csr(hpmcounter10);
	// mtvec_s           = read_csr(hpmcounter11);
	// mcounen_s         = read_csr(hpmcounter12);
	// mscratch_s        = read_csr(hpmcounter13);
	// mepc_s            = read_csr(hpmcounter14);
	// mtval_s           = read_csr(hpmcounter15);
	// mip_s             = read_csr(hpmcounter16);
	// mtval2_s          = read_csr(hpmcounter17);
	// menvcfg_s         = read_csr(hpmcounter18);
	// mseccfg_s         = read_csr(hpmcounter19);
	// pmpcfg0_s         = read_csr(hpmcounter20);
	// pmpaddr0_s        = read_csr(hpmcounter21);
	// mcountinh_s       = read_csr(hpmcounter22);
	// fflags_s          = read_csr(hpmcounter23);
  }
  
  void perfend(){
	for(int i = 0; i < totalcsrperf; i++){
		csr_read_e[i] = ghe_csr_perf_read(i) - csr_read_s[i];
	}
	// total_cycle_e     = read_csr(cycle) - total_cycle_s;
	// total_commit_e    = read_csr(hpmcounter3)  - total_commit_s  ;
	// total_csr_e       = read_csr(hpmcounter4)  - total_csr_s   ;
	// csrw_e            = read_csr(hpmcounter5)  - csrw_s  ;
	// mstatus_e         = read_csr(hpmcounter6)  - mstatus_s   ; 
	// misa_e            = read_csr(hpmcounter7)  - misa_s  ; 
	// medeleg_e         = read_csr(hpmcounter8)  - medeleg_s     ;   
	// mideleg_e         = read_csr(hpmcounter9)  - mideleg_s     ; 
	// mie_e             = read_csr(hpmcounter10) - mie_s    ;         
	// mtvec_e           = read_csr(hpmcounter11) - mtvec_s   ;     
	// mcounen_e         = read_csr(hpmcounter12) - mcounen_s    ; 
	// mscratch_e        = read_csr(hpmcounter13) - mscratch_s    ;
	// mepc_e            = read_csr(hpmcounter14) - mepc_s    ;
	// mtval_e           = read_csr(hpmcounter15) - mtval_s    ;
	// mip_e             = read_csr(hpmcounter16) - mip_s    ;
	// mtval2_e          = read_csr(hpmcounter17) - mtval2_s    ;
	// menvcfg_e         = read_csr(hpmcounter18) - menvcfg_s    ;
	// mseccfg_e         = read_csr(hpmcounter19) - mseccfg_s    ;
	// pmpcfg0_e         = read_csr(hpmcounter20) - pmpcfg0_s    ;
	// pmpaddr0_e        = read_csr(hpmcounter21) - pmpaddr0_s    ;
	// mcountinh_e       = read_csr(hpmcounter22) - mcountinh_s    ;
	// fflags_e          = read_csr(hpmcounter23) - fflags_s;
  }
  

void r_ini (void)
{	
   	//================== Initialisation ==================//
    // Bound current thread to BOOM
    // if (gc_pthread_setaffinity(BOOM_ID) != 0) {
	// 	printf ("[Boom-C%x]: pthread_setaffinity failed.", BOOM_ID);
	// }
	// else{
	// 	printf ("[Boom-C%x]: pthread_setaffinity successful.\n", BOOM_ID);
	// }
	
	start_tracing();
    /*=====================*/
    /*  GC configurations  */
    /*=====================*/
    ghm_cfg_agg(AGG_CORE_ID);
    ght_set_numberofcheckers(NUM_CORES-1);


    // Insepct load operations 
    // GID: 0x01 
    // Func: 0x00; 0x01; 0x02; 0x03; 0x04; 0x05; 0x06
    // Opcode: 0x03
    // Data path: LDQ - 0x02
    ght_cfg_filter(0x01, 0x00, 0x03, 0x02); // lb
    ght_cfg_filter(0x01, 0x01, 0x03, 0x02); // lh
    ght_cfg_filter(0x01, 0x02, 0x03, 0x02); // lw
    ght_cfg_filter(0x01, 0x04, 0x03, 0x02); // lbu
    ght_cfg_filter(0x01, 0x05, 0x03, 0x02); // lhu
    ght_cfg_filter(0x01, 0x03, 0x03, 0x02); // ld
    ght_cfg_filter(0x01, 0x06, 0x03, 0x02); // lwu
    // Func: 0x02; 0x03; 0x04
    // Opcode: 0x07
    // Data path: LDQ - 0x02
    ght_cfg_filter(0x01, 0x02, 0x07, 0x02); // flw
    ght_cfg_filter(0x01, 0x03, 0x07, 0x02); // fld
    ght_cfg_filter(0x01, 0x04, 0x07, 0x02); // flq
    // C.load operations 
    // GID: 0x01
    // Func: 0x02; 0x03; 0x04; 0x05; 0x06; 0x07
    // Opcode: 0x0
    // MSB: 0
    ght_cfg_filter_rvc(0x01, 0x02, 0x00, 0x00, 0x02); // c.fld, c.lq
    ght_cfg_filter_rvc(0x01, 0x03, 0x00, 0x00, 0x02); // c.fld, c.lq
    ght_cfg_filter_rvc(0x01, 0x04, 0x00, 0x00, 0x02); // c.lw
    ght_cfg_filter_rvc(0x01, 0x05, 0x00, 0x00, 0x02); // c.lw
    ght_cfg_filter_rvc(0x01, 0x06, 0x00, 0x00, 0x02); // c.flw, c.ld
    ght_cfg_filter_rvc(0x01, 0x07, 0x00, 0x00, 0x02); // c.flw, c.ld

    // C.lsp operations
    // GID: 0x01
    // Func: 0x02; 0x03; 0x04; 0x05; 0x06; 0x07
    // Opcode: 0x02
    // MSB:0
    ght_cfg_filter_rvc(0x01, 0x02, 0x02, 0x00, 0x02); // c.fldsp, c.lqsp
    ght_cfg_filter_rvc(0x01, 0x03, 0x02, 0x00, 0x02); // c.fldsp, c.lqsp 
    ght_cfg_filter_rvc(0x01, 0x04, 0x02, 0x00, 0x02); // c.lwsp
    ght_cfg_filter_rvc(0x01, 0x05, 0x02, 0x00, 0x02); // c.lwsp
    ght_cfg_filter_rvc(0x01, 0x06, 0x02, 0x00, 0x02); // c.flwsp, c.ldsp
    ght_cfg_filter_rvc(0x01, 0x07, 0x02, 0x00, 0x02); // c.flwsp, c.ldsp

    // Insepct store operations 
    // GID: 0x02
    // Func: 0x00; 0x01; 0x02; 0x03
    // Opcode: 0x23
    // Data path: STQ - 0x03
    ght_cfg_filter(0x02, 0x00, 0x23, 0x03); // sb
    ght_cfg_filter(0x02, 0x01, 0x23, 0x03); // sh
    ght_cfg_filter(0x02, 0x02, 0x23, 0x03); // sw
    ght_cfg_filter(0x02, 0x03, 0x23, 0x03); // sd
    // Func: 0x02; 0x03; 0x04
    // Opcode: 0x27
    // Data path: LDQ - 0x02
    ght_cfg_filter(0x02, 0x02, 0x27, 0x03); // fsw
    ght_cfg_filter(0x02, 0x03, 0x27, 0x03); // fsd
    ght_cfg_filter(0x02, 0x04, 0x27, 0x03); // fsq
    // C.sotre operations 
    // GID: 0x02
    // Func: 0x02; 0x03; 0x04; 0x05; 0x06; 0x07
    // Opcode: 0x00
    // MSB: 1
    ght_cfg_filter_rvc(0x02, 0x02, 0x00, 0x01, 0x03); // c.fsd, c.sq
    ght_cfg_filter_rvc(0x02, 0x03, 0x00, 0x01, 0x03); // c.fsd, c.sq
    ght_cfg_filter_rvc(0x02, 0x04, 0x00, 0x01, 0x03); // c.sw
    ght_cfg_filter_rvc(0x02, 0x05, 0x00, 0x01, 0x03); // c.sw
    ght_cfg_filter_rvc(0x02, 0x06, 0x00, 0x01, 0x03); // c.fsw, c.sd
    ght_cfg_filter_rvc(0x02, 0x07, 0x00, 0x01, 0x03); // c.fsw, c.sd

    // C.ssp operations
    // GID: 0x02
    // Func: 0x02; 0x03; 0x04; 0x05; 0x06; 0x07
    // Opcode: 0x02
    // MSB:0
    ght_cfg_filter_rvc(0x02, 0x02, 0x02, 0x01, 0x03); // c.fsdsp, c.sqsp
    ght_cfg_filter_rvc(0x02, 0x03, 0x02, 0x01, 0x03); // c.fsdsp, c.sqsp
    ght_cfg_filter_rvc(0x02, 0x04, 0x02, 0x01, 0x03); // c.swsp
    ght_cfg_filter_rvc(0x02, 0x05, 0x02, 0x01, 0x03); // c.swsp
    ght_cfg_filter_rvc(0x02, 0x06, 0x02, 0x01, 0x03); // c.fswsp, c.sdsp
    ght_cfg_filter_rvc(0x02, 0x07, 0x02, 0x01, 0x03); // c.fswsp, c.sdsp

    // Insepct CSR read operations
    // GID: 0x01
    // Func: 0x02
    // Opcode: 0x73
    // Data path: PRFs - 0x01
    ght_cfg_filter(0x03, 0x01, 0x73, 0x01);
    ght_cfg_filter(0x03, 0x02, 0x73, 0x01);

    // Insepct atomic operations
    // GID: 0x2F
    // Func: 0x02; 0x03
    // Opcode: 0x2F
    // Data path: LDQ - 0x02
    ght_cfg_filter(0x01, 0x02, 0x2F, 0x05); // 32-bit
    ght_cfg_filter(0x01, 0x03, 0x2F, 0x05); // 64-bit

    // se: 00, end_id: 0x01, scheduling: rr, start_id: 0x01
    ght_cfg_se(0x00, 0x01, 0x01, 0x01);
    // se: 01, end_id: 0x02, scheduling: rr, start_id: 0x02
    ght_cfg_se(0x01, 0x02, 0x01, 0x02);
    // se: 02, end_id: 0x03, scheduling: rr, start_id: 0x03
    ght_cfg_se(0x02, 0x03, 0x01, 0x03);
    // se: 03, end_id: 0x04, scheduling: rr, start_id: 0x04
    ght_cfg_se(0x03, 0x04, 0x01, 0x04);

    // Map: GIDs for cores
    for (int i = 1; i < NUM_CORES; i++){
        r_set_corex_p_s(i);
    }

    // Shared snapshots
    // Revisit: makeing below code to be generic
    // dual core
    // ght_cfg_mapper (0b00001111, 0b0011);
    // ght_cfg_mapper (0b00010111, 0b0011);

    // ght_cfg_mapper (0b00001111, 0b0101);
    // ght_cfg_mapper (0b00010111, 0b0011);
    // ght_cfg_mapper (0b00011111, 0b0110);
    
	start_tracing();
    ght_cfg_mapper (0b00001111, 0b1001);
    ght_cfg_mapper (0b00010111, 0b0011);
    ght_cfg_mapper (0b00011111, 0b0110);
    ght_cfg_mapper (0b00100111, 0b1100);
	start_tracing();

    /* Simulating a N-width filter*/
	ght_debug_filter_width (FILTERWIDTH);
	printf("[Boom-%x]: Initialisation for RELIABILITY is now completed, number of Checkers: %d!\r\n", BOOM_ID, NUM_CORES-1);
	printf("[Boom-%x]: Simulating %d-width event filter!\r\n", BOOM_ID, FILTERWIDTH);
}

void rStartup (void) __attribute__ ((constructor));
void rCleanup (void) __attribute__ ((destructor));

pthread_t threads[NUM_CORES-1];
struct timespec start, end;
int uart_lock;

void* thread_r_checker(void* args){
    uint64_t hart_id = (uint64_t) args;
    //================== Initialisation ==================//
	if (gc_pthread_setaffinity(hart_id) != 0){
		printf ("[Rocket-C%x]: pthread_setaffinity failed.", hart_id);
	}
	else{
		printf ("[Rocket-C%x]: pthread_setaffinity successful.\n", hart_id);
	}
	
	start_tracing();
	ghe_asR();
	ght_set_satp_priv();

	// while (ghe_checkght_status() != 0x04) {
	// }

	ghe_go();
	ghe_initailised(1);

	// ROCC_INSTRUCTION_S (1, 0X01, 0x69);
	//======================= Perf ========================//
	ghe_perf_ctrl(0x01);
  	ghe_perf_ctrl(0x00);

	//===================== Execution =====================//
	ROCC_INSTRUCTION (1, 0x75); // Record context
	ROCC_INSTRUCTION (1, 0x73); // Store context from main core
	ROCC_INSTRUCTION (1, 0x64); // Record PC

	/*
	for (int sel_elu = 0; sel_elu < 2; sel_elu ++) {
    	ROCC_INSTRUCTION_S (1, sel_elu, 0x65);

		while (elu_checkstatus() != 0){
			// printf("C%x: Error detected for ELU %x.\r\n", hart_id, sel_elu);
			ROCC_INSTRUCTION_S (1, sel_elu, 0x63);
		}
  	}
	*/

	while (ghe_checkght_status() != 0x02) {
		if ((ghe_rsur_status() & 0x18) == 0x08) {
			ROCC_INSTRUCTION (1, 0x60);
			R_INSTRUCTION_JLR (3, 0x00);
		}
  	}
	//=================== Post execution ===================//
	__asm__ volatile("nop");
	__asm__ volatile("nop");
	__asm__ volatile("nop");
	__asm__ volatile("nop");

	ROCC_INSTRUCTION (1, 0x72); // Store context from checker core
	ROCC_INSTRUCTION (1, 0x60);

	__asm__ volatile("nop");
	__asm__ volatile("nop");
	__asm__ volatile("nop");
	__asm__ volatile("nop");

	ghe_perf_ctrl(0x07<<1);
	uint64_t perf_val_NCP = ghe_perf_read();
	ghe_perf_ctrl(0x01<<1);
	uint64_t perf_val_NC = ghe_perf_read();
	ghe_perf_ctrl(0x02<<1);
	uint64_t perf_val_NPC = ghe_perf_read();
	ghe_perf_ctrl(0x03<<1);
	uint64_t perf_val_NOT = ghe_perf_read();
	ghe_perf_ctrl(0x04<<1);
	uint64_t perf_val_NNC = ghe_perf_read();
	ghe_perf_ctrl(0x05<<1);
	uint64_t Nonchecking_OtherThreads = ghe_perf_read();
	ghe_perf_ctrl(0x06<<1);
	uint64_t perf_val_blocking_RAM = ghe_perf_read();
	ghe_perf_ctrl(0x08<<1);
	uint64_t Nonchecking_MCheck = ghe_perf_read();
	ghe_perf_ctrl(0x09<<1);
	uint64_t Insts = ghe_perf_read();
	ghe_perf_ctrl(0x0a<<1);
	uint64_t CPSTrans = ghe_perf_read();
	ghe_perf_ctrl(0x0b<<1);
	uint64_t Nonchecking_Sched = ghe_perf_read();
	ghe_perf_ctrl(0x0c<<1);
	uint64_t Execution_time = ghe_perf_read();
	


	
	// lock_acquire(&uart_lock);
	printf("================ PERF: C%x ================\r\n", hart_id);
	printf("[Rocket-C%x]: Perf: N.Execution time = %ld \r\n", hart_id, Execution_time);
	printf("[Rocket-C%x]: Perf: N.CheckPoints = %ld \r\n", hart_id, perf_val_NCP);
	printf("[Rocket-C%x]: Perf: N.CheckingState = %ld \r\n", hart_id, perf_val_NC);
	printf("[Rocket-C%x]: Perf: N.PostcheckingState = %ld \r\n", hart_id, perf_val_NPC);
	printf("[Rocket-C%x]: Perf: N.OtherThreads = %ld \r\n", hart_id, perf_val_NOT);
	printf("[Rocket-C%x]: Perf: N.Idle = %ld \r\n", hart_id, perf_val_NNC);
	printf("[Rocket-C%x]: Perf: N.Nonchecking_OtherThreads = %ld \r\n", hart_id, Nonchecking_OtherThreads);
	printf("[Rocket-C%x]: Perf: N.Nonchecking_MOtherThreads = %ld \r\n", hart_id, perf_val_blocking_RAM);
	printf("[Rocket-C%x]: Perf: N.Nonchecking_MCheck = %ld \r\n", hart_id, Nonchecking_MCheck);
	printf("[Rocket-C%x]: Perf: N.Nonchecking_Sched = %ld \r\n", hart_id, Nonchecking_Sched);
	printf("[Rocket-C%x]: Perf: N.CPSTrans = %ld \r\n", hart_id, CPSTrans);
	printf("[Rocket-C%x]: Perf: N.Insts = %ld \r\n", hart_id, Insts);
	// lock_release(&uart_lock);
	

	// lock_acquire(&uart_lock);
	// printf("================ PERF: C%x ================\r\n", hart_id);
	// printf("[Rocket-C%x]: Perf: N.Debug_perf_blocking_CP = %ld \r\n", hart_id, perf_val_NCP);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_id_ex_h = %ld \r\n", hart_id, perf_val_NC);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_id_mem_h = %ld \r\n", hart_id, perf_val_NPC);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_id_wb_h = %ld \r\n", hart_id, perf_val_NOT);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_id_sb_h = %ld \r\n", hart_id, perf_val_NNC);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_CSR_S = %ld \r\n", hart_id, Nonchecking_OtherThreads);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_FP = %ld \r\n", hart_id, Nonchecking_MOtherThreads);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_MEM = %ld \r\n", hart_id, Nonchecking_MCheck);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_DIV = %ld \r\n", hart_id, Insts);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_ST = %ld \r\n", hart_id, CPSTrans);
	// printf("[Rocket-C%x]: Perf: N.debug_perf_blocking_FENCE = %ld \r\n", hart_id, Nonchecking_Sched);
	// lock_release(&uart_lock);
	
	ghe_initailised(0);
	ghe_release();
  	ght_unset_satp_priv();
	ghe_asG();

	__asm__ volatile("nop");
	end_tracing();
	return NULL;
}



void rStartup (void) {
    //================== Initialisation ==================//
    //Bound current thread to BOOM
    if (gc_pthread_setaffinity(BOOM_ID) != 0) {
		printf ("[Boom-C%x]: pthread_setaffinity failed.", BOOM_ID);
	}
	else{
		printf ("[Boom-C%x]: pthread_setaffinity successful.\n", BOOM_ID);
	}
	
	

    // GC threads
	// pthread_create(&threads[0], NULL, thread_r_checker, (void *) (1));
	// pthread_create(&threads[1], NULL, thread_r_checker, (void *) (2));
	// pthread_create(&threads[2], NULL, thread_r_checker, (void *) (3));
	// pthread_create(&threads[3], NULL, thread_r_checker, (void *) (4));
    for (uint64_t i = 0; i < NUM_CORES - 1; i++) {
		pthread_create(&threads[i], NULL, thread_r_checker, (void *) (i+1));
	}

	start_tracing();
    perfstart();
	r_ini();

	ROCC_INSTRUCTION (1, 0x34);
	while (ght_get_initialisation() == 0){
 	}

	ght_set_satp_priv();
	// printf("[Boom-%x]: Test is now started: \r\n", BOOM_ID);

	//======================= Perf ========================//
	ghe_perf_ctrl(0x01);
  	ghe_perf_ctrl(0x00); 
	clock_gettime(CLOCK_MONOTONIC_RAW, &start); // get start time

    // ROCC_INSTRUCTION_S (1, 0x3, 0x69);
   	ROCC_INSTRUCTION (1, 0x31); // start monitoring
   	ROCC_INSTRUCTION_S (1, 0x01, 0x70); // ISAX_Go
    //===================== Execution =====================//

}


void rCleanup (void){
	//=================== Post execution ===================//
	ROCC_INSTRUCTION (1, 0x32); // stop monitoring
	ROCC_INSTRUCTION_S (1, 0X02, 0x70); // ISAX_Stop
	__asm__ volatile("nop");
	__asm__ volatile("nop");
	__asm__ volatile("nop");
	__asm__ volatile("nop");

	perfend();
	clock_gettime(CLOCK_MONOTONIC_RAW, &end);

	ghe_perf_ctrl(0x07<<1);
	uint64_t perf_val_CC = ghe_perf_read();
	ghe_perf_ctrl(0x01<<1);
	uint64_t perf_val_SB = ghe_perf_read();
	ghe_perf_ctrl(0x02<<1);
	uint64_t perf_val_SS = ghe_perf_read();
	ghe_perf_ctrl(0x03<<1);
	uint64_t perf_val_CS = ghe_perf_read();
	ghe_perf_ctrl(0x04<<1);
	uint64_t perf_val_SS_OT = ghe_perf_read();
	ghe_perf_ctrl(0x05<<1);
	uint64_t perf_val_SS_AB = ghe_perf_read();
	ghe_perf_ctrl(0x06<<1);
	uint64_t perf_val_OT = ghe_perf_read();
	ghe_perf_ctrl(0x08<<1);
	uint64_t perf_val_rsustall = ghe_perf_read();
	ghe_perf_ctrl(0x09<<1);
	uint64_t perf_val_gh_stall = ghe_perf_read();
	ghe_perf_ctrl(0x0a<<1);
	uint64_t perf_val_inst_commit = ghe_perf_read();
	ghe_perf_ctrl(0x0b<<1);
	uint64_t perf_val_kernel_inst_commit = ghe_perf_read();
	ghe_perf_ctrl(0x0c<<1);
	uint64_t perf_val_exception_counter = ghe_perf_read();
	ghe_perf_ctrl(0x0d<<1);
	uint64_t perf_val_interrupt_counter = ghe_perf_read();
	ghe_perf_ctrl(0x0e<<1);
	uint64_t perf_val_stallexception_counter = ghe_perf_read();
	ghe_perf_ctrl(0x0f<<1);
	uint64_t perf_val_stallinterrupt_counter = ghe_perf_read();

	uint64_t bp_checker  = debug_bp_checker();
    uint64_t bp_cdc = debug_bp_cdc();
    uint64_t bp_filter = debug_bp_filter();

    printf("inst commit %lu %lu, csrw commit %lu %lu\n \
        mstatus write %lu %lu, misa %lu %lu\n \
        medeleg %lu %lu, mideleg %lu %lu\n \
        mie %lu %lu, mtvec %lu %lu\n \
        mcounen %lu %lu, mscratch %lu %lu\n \
        mepc %lu %lu, mcause %lu %lu\n \
        mtval %lu %lu, mip %lu %lu\n \
        mtval2 %lu %lu, menvcfg %lu %lu\n \
        mseccfg %lu %lu, pmpcfg0 %lu %lu\n \
        pmpaddr0 %lu %lu %lu %lu %lu %lu %lu %lu / %lu %lu %lu %lu %lu %lu %lu %lu\n \
        mcountinh %lu %lu\n \
        mevents %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu\n \
	    mevents_s %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu %lu\n \
		mncause %lu %lu, mnepc %lu %lu, mnscratch %lu %lu, mnstatus %lu %lu\n \
        dcsr %lu %lu, dpc %lu %lu, dscratch0 %lu %lu, dscratch1 %lu %lu\n \
        tselect %lu %lu, tdata1 %lu %lu, tdata2 %lu %lu, tdata3 %lu %lu, mcontext %lu %lu\n \
        fflags %lu %lu, fcsr %lu %lu, frm %lu %lu\n \
		sstatus %lu %lu, sie %lu %lu, stvec %lu %lu, sounteren %lu %lu\n \
        senvcfg %lu %lu\n \
        sscratch %lu %lu, sepc %lu %lu, scause %lu %lu, stval %lu %lu, sip %lu %lu\n \
        satp %lu %lu, scontext %lu %lu\n \
        ",csr_read_e[0], csr_read_s[0], csr_read_e[1], csr_read_s[1],
        csr_read_e[2], csr_read_s[2], csr_read_e[3], csr_read_s[3],
        csr_read_e[4], csr_read_s[4], csr_read_e[5], csr_read_s[5],
        csr_read_e[6], csr_read_s[6], csr_read_e[7], csr_read_s[7],
        csr_read_e[8], csr_read_s[8], csr_read_e[9], csr_read_s[9],
        csr_read_e[10], csr_read_s[10], csr_read_e[11], csr_read_s[11],
        csr_read_e[12], csr_read_s[12], csr_read_e[13], csr_read_s[13],
        csr_read_e[14], csr_read_s[14], csr_read_e[15], csr_read_s[15],
        csr_read_e[16], csr_read_s[16], csr_read_e[17], csr_read_s[17],
        csr_read_e[18], csr_read_e[19], csr_read_e[20], csr_read_e[21], csr_read_e[22], csr_read_e[23], csr_read_e[24], csr_read_e[25], csr_read_s[18], csr_read_s[19], csr_read_s[20], csr_read_s[21], csr_read_s[22], csr_read_s[23], csr_read_s[24], csr_read_s[25],
        csr_read_e[26], csr_read_s[26],
        csr_read_e[27], csr_read_e[28], csr_read_e[29], csr_read_e[30], csr_read_e[31], csr_read_e[32], csr_read_e[33], csr_read_e[34], csr_read_e[35], 
        csr_read_e[36], csr_read_e[37], csr_read_e[38], csr_read_e[39], csr_read_e[40], csr_read_e[41], csr_read_e[42], csr_read_e[43], csr_read_e[44],
        csr_read_e[45], csr_read_e[46], csr_read_e[47], csr_read_e[48], csr_read_e[49], csr_read_e[50], csr_read_e[51], csr_read_e[52], csr_read_e[53],
        csr_read_e[54], csr_read_e[55],
		csr_read_s[27], csr_read_s[28], csr_read_s[29], csr_read_s[30], csr_read_s[31], csr_read_s[32], csr_read_s[33], csr_read_s[34], csr_read_s[35], 
        csr_read_s[36], csr_read_s[37], csr_read_s[38], csr_read_s[39], csr_read_s[40], csr_read_s[41], csr_read_s[42], csr_read_s[43], csr_read_s[44],
        csr_read_s[45], csr_read_s[46], csr_read_s[47], csr_read_s[48], csr_read_s[49], csr_read_s[50], csr_read_s[51], csr_read_s[52], csr_read_s[53],
        csr_read_s[54], csr_read_s[55],

        csr_read_e[56], csr_read_s[56], csr_read_e[57], csr_read_s[57], csr_read_e[58], csr_read_s[58], csr_read_e[59], csr_read_s[59], 
		csr_read_e[60], csr_read_s[60], csr_read_e[61], csr_read_s[61], csr_read_e[62], csr_read_s[62], csr_read_e[63], csr_read_s[63], 
		csr_read_e[64], csr_read_s[64], csr_read_e[65], csr_read_s[65], csr_read_e[66], csr_read_s[66], csr_read_e[67], csr_read_s[67], csr_read_e[68], csr_read_s[68],
		
		csr_read_e[69], csr_read_s[69], csr_read_e[70], csr_read_s[70], csr_read_e[71], csr_read_s[71], 

		csr_read_e[72], csr_read_s[72], csr_read_e[73], csr_read_s[73], csr_read_e[74], csr_read_s[74], csr_read_e[75], csr_read_s[75], 
		csr_read_e[76], csr_read_s[76], 
		csr_read_e[77], csr_read_s[77], csr_read_e[78], csr_read_s[78], csr_read_e[79], csr_read_s[79], csr_read_e[80], csr_read_s[80], csr_read_e[81], csr_read_s[81], 
		csr_read_e[82], csr_read_s[82], csr_read_e[83], csr_read_s[83]
    );
	
	uint64_t status;
  	while (ght_get_initialisation() != 0) {

  	}

    // GC threads.
	for (uint64_t i = 0; i < NUM_CORES-1; i++) {
		pthread_join(threads[i], NULL);
	}

	// lock_acquire(&uart_lock);
	printf("================ PERF: BOOM%x ================\r\n", BOOM_ID);
	printf("[Boom-%x]: Perf: ExecutionTime = %ld \r\n", BOOM_ID, perf_val_CC);
	printf("[Boom-%x]: Perf: TotalInstCommit = %ld \r\n", BOOM_ID, perf_val_inst_commit);
	printf("[Boom-%x]: Perf: KernelInstCommit = %ld \r\n", BOOM_ID, perf_val_kernel_inst_commit);
	printf("[Boom-%x]: Perf: TotalException = %ld \r\n", BOOM_ID, perf_val_exception_counter);
	printf("[Boom-%x]: Perf: Interrupt = %ld \r\n", BOOM_ID, perf_val_interrupt_counter);
	printf("[Boom-%x]: Perf: StallException = %ld \r\n", BOOM_ID, perf_val_stallexception_counter);
	printf("[Boom-%x]: Perf: StallInterrupt = %ld \r\n", BOOM_ID, perf_val_stallinterrupt_counter);
	printf("[Boom-%x]: Perf: SchedulingStateTime = %ld \r\n", BOOM_ID, perf_val_SS);
	printf("[Boom-%x]: Perf: SchedulingBlockTime-AllBusy = %ld \r\n", BOOM_ID, perf_val_SS_AB);
	printf("[Boom-%x]: Perf: SchedulingBlockTime-OtherThread = %ld \r\n", BOOM_ID, perf_val_SS_OT);
	printf("[Boom-%x]: Perf: ICmaterBlockTime = %ld \r\n", BOOM_ID, perf_val_SB);
	printf("[Boom-%x]: Perf: RSUBlockTime = %ld \r\n", BOOM_ID, perf_val_rsustall);
	printf("[Boom-%x]: Perf: GHBlockTime = %ld \r\n", BOOM_ID, perf_val_gh_stall);
	printf("[Boom-%x]: Perf: CheckingStateTime = %ld \r\n", BOOM_ID, perf_val_CS);
	printf("[Boom-%x]: Perf: OtherThreadTime = %ld \r\n", BOOM_ID, perf_val_OT);
	printf("[Boom-%x]: Perf-BPChecker: %ld cycles; \r\n",BOOM_ID, bp_checker);
 	printf("[Boom-%x]: Perf-BPCDC: %ld cycles; \r\n", BOOM_ID, bp_cdc);
 	printf("[Boom-%x]: Perf-BP-Filter: %ld cycles. \r\n", BOOM_ID, bp_filter);
	// lock_release(&uart_lock);

	printf("[Boom-%x]: Test is now completed: \r\n", BOOM_ID);
	double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9;
    printf("==== Execution time: %f seconds ==== \r\n", elapsed);


	// perfend();
	// printf("total cycle %lu\n \
	// 	total commit %lu\n \
	// 	csr commit %lu\n \
	// 	csrw commit %lu\n \
	// 	mstatus write %lu \n \
	// 	misa %lu \n \
	// 	medeleg   %lu \n \
	// 	mideleg %lu \n \
	// 	mie  %lu \n \
	// 	mtvec %lu \n \
	// 	mcounen %lu \n \
	// 	mscratch %lu \n \
	// 	mepc %lu \n \
	// 	mtval %lu \n \
	// 	mip %lu \n \
	// 	mtval2 %lu \n \
	// 	menvcfg %lu \n \
	// 	mseccfg %lu\n \
	// 	pmpcfg0 %lu\n \
	// 	pmpaddr0 %lu\n \
	// 	mcountinh %lu\n \
	// 	fflags %lu\n \
	// 	",total_cycle_e, total_commit_e, total_csr_e, csrw_e, mstatus_e, misa_e, medeleg_e, mideleg_e, mie_e, mtvec_e, mcounen_e, mscratch_e, mepc_e,
	// 	mtval_e, mip_e, mtval2_e, menvcfg_e, mseccfg_e, pmpcfg0_e, pmpaddr0_e, mcountinh_e, fflags_e
	//   );
	
	// ght_unset_satp_priv();
	ROCC_INSTRUCTION (1, 0x30); // reset monitoring
	end_tracing();
	// ROCC_INSTRUCTION_S (1, 0X00, 0x69);
}
