
# Based on https://stackoverflow.com/questions/6760685/creating-a-singleton-in-python
class Singleton(type):
    """
    This metaclass is used to create singleton classes. That is, a class whose metaclass is
    Singleton will only ever have at most one instance of its type in existence at any time.
    """
    _instances = {}
    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super().__call__(*args, **kwargs)
        return cls._instances[cls]
