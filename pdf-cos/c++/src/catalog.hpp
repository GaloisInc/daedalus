
class CatalogException : std::exception {
  const char *msg;
public:
  CatalogException(const char *msg) : msg(msg) {}

  const char *what() const throw () override { return msg; }
};

void check_catalog(bool text);
