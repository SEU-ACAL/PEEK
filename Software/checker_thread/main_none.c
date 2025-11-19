#ifndef _GNU_SOURCE
	#define _GNU_SOURCE             /* See feature_test_macros(7) */
#endif
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include "libraries/ght.h"
#include "libraries/gc_top.h"
#include "libraries/ghe.h"
#include <time.h>

struct timespec start, end;
uint64_t csr_cycle[2];
uint64_t csr_instret[2];


/* Apply the constructor attribute to myStartupFun() so that it
     is executed before main() */
void gcStartup (void) __attribute__ ((constructor));


 /* Apply the destructor attribute to myCleanupFun() so that it
    is executed after main() */
void gcCleanup (void) __attribute__ ((destructor));
 
void gcStartup (void)
{
    // Bound current thread to BOOM
    if (gc_pthread_setaffinity(0) != 0){
		printf ("[Boom-C%x]: Pthread_setaffinity failed.", BOOM_ID);
	} else{
		printf ("[Boom-C%x]: Initialised!\r\n", BOOM_ID);
	}

	csr_cycle[0] = read_csr_safe(cycle);
	csr_instret[0]  = read_csr_safe(instret);
	printf("Cycles: %ld \r\n", csr_cycle[0]);
	printf("Insts: %ld \r\n", csr_instret[0]);

	ghe_perf_ctrl(0x01);
  	ghe_perf_ctrl(0x00); 
	clock_gettime(CLOCK_MONOTONIC_RAW, &start); // get start time
}
  
void gcCleanup (void)
{	
	csr_cycle[1] = read_csr_safe(cycle);
	csr_instret[1]  = read_csr_safe(instret);
	clock_gettime(CLOCK_MONOTONIC_RAW, &end); // get end time
	double elapsed = (end.tv_sec - start.tv_sec) + (end.tv_nsec - start.tv_nsec) / 1e9; // calculate elapsed time in seconds

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

	uint64_t bp_checker  = debug_bp_checker();
    uint64_t bp_cdc = debug_bp_cdc();
    uint64_t bp_filter = debug_bp_filter();

	
	printf("================ PERF: BOOM%x ================\r\n", BOOM_ID);
	printf("[Boom-%x]: Perf: ExecutionTime = %ld \r\n", BOOM_ID, perf_val_CC);
	printf("[Boom-%x]: Perf: TotalInstCommit = %ld \r\n", BOOM_ID, perf_val_inst_commit);
	printf("[Boom-%x]: Perf: KernelInstCommit = %ld \r\n", BOOM_ID, perf_val_kernel_inst_commit);
	printf("[Boom-%x]: Perf: TotalException = %ld \r\n", BOOM_ID, perf_val_exception_counter);
	printf("[Boom-%x]: Perf: Interrupt = %ld \r\n", BOOM_ID, perf_val_interrupt_counter);
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
	
	

	printf("==== Execution time: %f seconds ==== \r\n", elapsed);
	printf("Cycles: %ld \r\n", csr_cycle[1]);
	printf("Insts: %ld \r\n", csr_instret[1]);

	printf("==== Insts v.s. Cycles ==== \r\n");
	int64_t cycle = csr_cycle[1] - csr_cycle[0];
	int64_t instret = csr_instret[1] - csr_instret[0];
	printf("Cycles: %ld \r\n", cycle);
	printf("Insts: %ld \r\n", instret);

	printf ("[Boom-C%x]: Completed!\r\n", BOOM_ID);
}