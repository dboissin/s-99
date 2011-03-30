package logic

import org.scalatest.Spec
import logic.S99Logic._

class TestS99Logic() extends Spec {
    
    describe("P46 : Truth tables for logical expressions.") {
        it("and(true, true) should be true") {
            expect(true) { and(true, true) }
        }
        
        it("xor(true, true) should be false") {
            expect(false) { xor(true, true) }
        }
    }
    
    // describe("P47 : Truth tables for logical expressions") {
    //      it() {
    //          expect() { table2((a: Boolean, b: Boolean) => a and (a or !b)) }
    //      }
    //  }
        
    describe("P49 : An 3-bit gray codes") {
        it("should be List(000, 001, 011, 010, 110, 111, 101, 100)") {
            expect(List("000", "001", "011", "010", "110", "111", "101", "100")) { grey(3) }
        }
    }
    
    describe("""P50 : Huffman code of List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))""") {
        it("should be List((a,0), (b,101), (c,100), (d,111), (e,1101), (f,1100))") {
            expect(List(("a","0"), ("c","100"), ("b","101"), ("f","1100"), ("e","1101"), ("d","111"))) {
                huffman(List(("a", 45), ("b", 13), ("c", 12), ("d", 16), ("e", 9), ("f", 5)))
            }
        }
    }
    
}