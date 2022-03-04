#pragma once

template<class T>
class Owned {
    T x;

public:
    // Consumes an owned value
    Owned(T x);

    // Frees owned value
    ~Owned();

    // Accesses underlying owned value
    T* operator->();

    T borrow() const;
};

template <class T>
Owned<T>::Owned(T x) : x(x) {}

template <class T>
Owned<T>::~Owned() { x.free(); }

template <class T>
T* Owned<T>::operator->() { return &x; }

template <class T>
T Owned<T>::borrow() const { return x; }
