#include "ops.h"

#define rotr(val,amt) (((val) >> (amt)) | ((val) << (8 - (amt))))

uint8_t op0(uint8_t bar, uint8_t foo) {
	return (foo ^ bar);
}
uint8_t op1(uint8_t bar, uint8_t foo) {
	return (foo + bar);
}
uint8_t op2(uint8_t bar, uint8_t foo) {
	return rotr(foo, bar & 7);
}
uint8_t op3(uint8_t bar, uint8_t foo) {
	return rotr(foo, (bar == 0));
}
uint8_t op4(uint8_t bar, uint8_t foo) {
	return rotr(foo == 0, bar == 0);
}
uint8_t op5(uint8_t bar, uint8_t foo) {
	return rotr(0xFF ^ foo, bar & 7);
}
uint8_t op6(uint8_t bar, uint8_t foo) {
	return (bar ^ (foo == 0));
}
uint8_t op7(uint8_t bar, uint8_t foo) {
	return (bar + (foo == 0));
}
uint8_t op8(uint8_t bar, uint8_t foo) {
	return (bar ^ (foo + bar));
}
uint8_t op9(uint8_t bar, uint8_t foo) {
	return (bar + (foo ^ bar));
}
uint8_t op10(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, (bar == 0)));
}
uint8_t op11(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, (bar & 7)));
}
uint8_t op12(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, (bar & 7)));
}
uint8_t op13(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, (bar == 0)));
}
uint8_t op14(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, 1));
}
uint8_t op15(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, 2));
}
uint8_t op16(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, 3));
}
uint8_t op17(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, 4));
}
uint8_t op18(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, 5));
}
uint8_t op19(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, 6));
}
uint8_t op20(uint8_t bar, uint8_t foo) {
	return (bar + rotr(foo, 7));
}
uint8_t op21(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, 1));
}
uint8_t op22(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, 2));
}
uint8_t op23(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, 3));
}
uint8_t op24(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, 4));
}
uint8_t op25(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, 5));
}
uint8_t op26(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, 6));
}
uint8_t op27(uint8_t bar, uint8_t foo) {
	return (bar ^ rotr(foo, 7));
}
uint8_t op28(uint8_t bar, uint8_t foo) {
	return (bar ^ (bar + rotr(foo, 5)));
}
uint8_t op29(uint8_t bar, uint8_t foo) {
	return (bar ^ (bar + rotr(foo, 3)));
}
uint8_t op30(uint8_t bar, uint8_t foo) {
	return (bar + (bar ^ rotr(foo, 3)));
}
uint8_t op31(uint8_t bar, uint8_t foo) {
	return (bar + (bar ^ rotr(foo, 5)));
}
uint8_t op32(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 3) ^ 0x1F;
}
uint8_t op33(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 5) ^ 0x07;
}
uint8_t op34(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 2) ^ 0x3F;
}
uint8_t op35(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 6) ^ 0x03;
}
uint8_t op36(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 2) ^ 0xC0;
}
uint8_t op37(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 5) ^ 0xF8;
}
uint8_t op38(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 6) ^ 0xFC;
}
uint8_t op39(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 3) ^ 0xE0;
}
uint8_t op40(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 6) ^ 0x01;
}
uint8_t op41(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 2) ^ 0x17;
}
uint8_t op42(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 3) ^ 0x0B;
}
uint8_t op43(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 5) ^ 0x02;
}
uint8_t op44(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 2) ^ 0x0D;
}
uint8_t op45(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 3) ^ 0x06;
}
uint8_t op46(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 4) ^ 0x03;
}
uint8_t op47(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 1) ^ 0x1B;
}
uint8_t op48(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 4) | 0x05) ^ 0x02;
}
uint8_t op49(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 3) | 0x0B) ^ 0x04;
}
uint8_t op50(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 5) | 0x02) ^ 0x01;
}
uint8_t op51(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 6) | 0x01);
}
uint8_t op52(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 1) | 0x1B) ^ 0x24;
}
uint8_t op53(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 7);
}
uint8_t op54(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 3) | 0x06) ^ 0x09;
}
uint8_t op55(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 5) | 0x01) ^ 0x02;
}
uint8_t op56(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 4) | 0x06) ^ 0x01;
}
uint8_t op57(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 5) | 0x03);
}
uint8_t op58(uint8_t bar, uint8_t foo) {
	return bar ^ rotr(foo, 6) ^ 0x01;
}
uint8_t op59(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 2) | 0x19) ^ 0x06;
}
uint8_t op60(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 4) | 0x0A) ^ 0x05;
}
uint8_t op61(uint8_t bar, uint8_t foo) {
	return foo ^ ((bar << 5) | 0x1F); // flipped
}
uint8_t op62(uint8_t bar, uint8_t foo) {
	return bar ^ ((foo << 3) + 0x07);
}
uint8_t op63(uint8_t bar, uint8_t foo) {
	return bar ^ (rotr(foo, 6) | 0x02) ^ 0x01;
}

op_func opsRB2[32] = {
	op0, op1, op2, op3, op4, op5, op6, op7, op8, op9,
	op10, op11, op12, op13, op14, op15, op16, op17, op18, op19,
	op20, op21, op22, op23, op24, op25, op26, op27, op28, op29,
	op30, op31
};

op_func opsRB3[32] = {
	op32, op33, op34, op35, op36, op37, op38, op39, op40, op41,
	op42, op43, op44, op45, op46, op47, op48, op49, op50, op51,
	op52, op53, op54, op55, op56, op57, op58, op59, op60, op61,
	op62, op63
};