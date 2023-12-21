import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import '../skyve_textfield.dart';
import '../../util/validators.dart';

class IntTextField extends StatelessWidget {
  static final TextInputFormatter integerFormatter =
      FilteringTextInputFormatter.allow(RegExp(r'[\-0-9]'));

  final String? label;
  final String propertyKey;
  late final List<Validator> validators;

  IntTextField({
    super.key,
    this.label,
    required this.propertyKey,
    List<Validator> validators = const [],
  }) {
    this.validators = List.from(validators)..add(IntFieldValidator());
  }

  @override
  Widget build(BuildContext context) {
    return SkyveTextField(
      propertyKey: propertyKey,
      label: label,
      validators: validators,
      keyboardType: const TextInputType.numberWithOptions(signed: true),
      inputFormatters: [integerFormatter],
    );
  }
}

class IntFieldValidator extends Validator {
  static const errorMSg = 'Must be a whole number.';
  static const int _MAX_VALUE = 2147483647;
  static const int _MIN_VALUE = -2147483648;

  @override
  String? validate(String? value) {
    if ((value ?? '') == '') {
      return null;
    }

    int? parseResult = int.tryParse(value!);
    if (parseResult == null) {
      // Couldn't parse as an int
      return errorMSg;
    } else if (parseResult > _MAX_VALUE || parseResult < _MIN_VALUE) {
      // Out of (Java's) range
      return errorMSg;
    }

    // All good!
    return null;
  }
}
