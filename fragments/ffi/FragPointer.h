#ifndef FRAGPOINTER_DEFINED
#define FRAGPOINTER_DEFINED
extern void nimPointerDeleter(void* ptr);
template<class T>
class FragPointer {
public:
  FragPointer(): ptr(nullptr) {};
  explicit FragPointer(T* ptr) noexcept : ptr(ptr) {};
  FragPointer(FragPointer&& p) noexcept { free(); ptr = p.ptr; p.ptr = nullptr; };

  ~FragPointer() { free(); };
  T* get() { return ptr; }
  const T* get() const { return ptr; }
  T* release() { T* tmp = ptr; ptr = nullptr; return tmp; }
  operator T*() { return ptr; }
  FragPointer& operator =(T* new_ptr) noexcept { free(); ptr = new_ptr; return *this; }
  FragPointer& operator =(FragPointer&& p) noexcept { free(); ptr = p.ptr; p.ptr = nullptr; return *this; }
  T* operator ->() { return ptr; }
  explicit operator bool() const { return ptr != nullptr; }

private:
  void free() { if(ptr) nimPointerDeleter(ptr); }
  T* ptr = nullptr;
};
#endif // FRAGPOINTER_DEFINED