# Cerus Compiler

> **This project is in a pre-alpha stage.**  
> Once I am able to compile Rust HIR to RISC-V, the project will move into the **Alpha phase**.

---

## 🔧 What is Cerus?

**Cerus** is a **mathematically proven compiler** designed for the **guaranteed compilation of Rust**.  
It is heavily inspired by [CompCert](https://github.com/AbsInt/CompCert), a formally verified C compiler.

---

## 🚀 Why I'm Creating This

### 🧠 1. Rust's Memory Model Is Still Underspecified

Rust claims to be **memory safe**, but [they haven't formally defined the memory model yet](https://doc.rust-lang.org/stable/reference/memory-model.html).  
Our goal is to:

- Define a **mathematically proven memory model**
- Keep it **simple, well-documented**, and **formally verified**

Fortunately, [good research](https://plv.mpi-sws.org/rustbelt/) has already been done on subsets of Rust (like **RustBelt**), which I plan to incorporate.

---

### 🧩 2. Move Away from LLVM Limitations

While **rustc** compiles to LLVM, LLVM:

- Doesn't support as many targets as GCC
- Lacks versatility in some embedded and legacy hardware contexts

Cerus aims to:

- **Preserve Rust's features**
- **Match GCC's hardware reach**
- Provide a **fully verified compilation pipeline**

---

### 🔬 3. Compilers Are Too Important to Trust Blindly

> "Compilers are the most important part of programming."

- **Rustc** is already **vigorously tested**
- But **testing can't guarantee the absence of bugs**

The solution? **Mathematics!**

Cerus will use **formal proofs** to **guarantee correctness** of the compilation process.

---

## 🛠 How to Build

```sh
make              # Compile the project
make clean        # Clean build artifacts
make dump-conflicts  # Dump Menhir conflicts
