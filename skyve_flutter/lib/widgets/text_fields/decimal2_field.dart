import 'package:flutter/material.dart';
import 'package:flutter/services.dart';

import '../../util/validators.dart';
import '../skyve_textfield.dart';

class Decimal2Field extends StatelessWidget {
  // TODO this'll likely be the same across the various decimal fields
  static final TextInputFormatter decimalFormatter =
      FilteringTextInputFormatter.allow(RegExp(r'[\-0-9.]'));

  final String? label;
  final String propertyKey;
  late final List<Validator> validators;

  Decimal2Field({
    super.key,
    this.label,
    required this.propertyKey,
    List<Validator> validators = const [],
  }) {
    this.validators = List.of(validators)..add(Decimal2FieldValidator());
  }

  @override
  Widget build(BuildContext context) {
    return SkyveTextField(
      propertyKey: propertyKey,
      label: label,
      validators: validators,
      keyboardType: const TextInputType.numberWithOptions(signed: true),
      inputFormatters: [decimalFormatter],
    );
  }
}

class Decimal2FieldValidator extends Validator {
  static const String errorMsg = 'Invalid value.';

  @override
  String? validate(String? value) {
    if (value == null || value == '') return null;

    double? parseResult = double.tryParse(value);
    if (parseResult == null) {
      return errorMsg;
    }

    // Precision check?

    return null;
  }
}
