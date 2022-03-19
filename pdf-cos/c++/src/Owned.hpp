#pragma once


template <class T>
class Owned;

template <class T>
Owned<T> owned(T x);

template <class T>
Owned<T> borrowed(T x);

template<class T>
class Owned {
    T obj;

    // Owns x
    explicit Owned(T x);
    Owned() = delete;

public:

    Owned(const Owned<T> &x);
    //Owned(Owned &&x);

    // Frees owned value
    ~Owned();

    Owned &operator=(const Owned &x);

    // Accesses underlying owned value
    T* operator->();
    T const* operator->() const;

    T borrow() const;
    T get();

    friend Owned<T> owned<T>(T);
    friend Owned<T> borrowed<T>(T);
};

template <class T>
T Owned<T>::get()
{
    obj.copy();
    return obj;
}

// Takes ownership of its argument
template <class T>
Owned<T> owned(T x)
{
    return Owned{x};
}

// Borrows its argument
template <class T>
Owned<T> borrowed(T x)
{
    x.copy();
    return Owned{x};
}

template <class T>
Owned<T>::Owned(const Owned<T> &x) : obj(x.obj)
{
    obj.copy();
}

//template <class T>
//Owned<T>::Owned(Owned<T> &&x) : obj(x.obj) {}

template <class T>
Owned<T>::Owned(T x) : obj(x) {}

template <class T>
Owned<T> &Owned<T>::operator=(const Owned<T> &rhs)
{
    obj.free();
    obj = rhs.obj;
    obj.copy();
    return *this;
}

template <class T>
Owned<T>::~Owned()
{
    obj.free();
}

template <class T>
T* Owned<T>::operator->() {
    return &obj;
}

template <class T>
T const* Owned<T>::operator->() const {
    return &obj;
}

template <class T>
T Owned<T>::borrow() const {
    return obj;
}
