class DelegatingValidator {
  final List<Validator> _delegates;

  const DelegatingValidator(this._delegates);

  String? validate(String? value) {
    List<String> msgs = [];

    for (var delegate in _delegates) {
      String? newMsg = delegate.validate(value);
      if (newMsg != null) {
        msgs.add(newMsg);

        if (!delegate.continueOnFail) {
          break;
        }
      }
    }

    if (msgs.isEmpty) {
      return null;
    } else {
      return msgs.join("; ");
    }
  }

  @override
  String toString() {
    return "DelegatingValidator[$_delegates]";
  }
}

abstract class Validator {
  String? validate(String? value);
  bool get continueOnFail => true;
}