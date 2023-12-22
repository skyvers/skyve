class DelegatingValidator extends Validator {
  final List<Validator> _delegates;

  DelegatingValidator(this._delegates);

  @override
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

class NoOpValidator extends Validator {
  @override
  String? validate(String? value) => null;
}

class RequiredValidator extends Validator {
  String? formLabel;

  RequiredValidator(this.formLabel);

  @override
  bool get continueOnFail => false;

  @override
  String? validate(String? value) {
    if ((value ?? '').isEmpty) {
      if (formLabel == null) {
        return 'An answer is required';
      } else {
        return '$formLabel is required';
      }
    } else {
      // No issue
      return null;
    }
  }
}
