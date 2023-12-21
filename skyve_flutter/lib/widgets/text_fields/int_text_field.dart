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
  @override
  String? validate(String? value) {
    if ((value ?? '') == '') {
      return null;
    }

    // FIXME limit to Java's MAX_INT?
    if (int.tryParse(value!) == null) {
      return 'Must be a whole number.';
    }

    return null;
  }
}
