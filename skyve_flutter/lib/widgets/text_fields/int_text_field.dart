import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:skyve_flutter/widgets/skyve_textfield.dart';

class IntTextField extends StatelessWidget {
  static final TextInputFormatter integerFormatter =
      FilteringTextInputFormatter.allow(RegExp(r'[\-0-9]'));

  final String? label;
  final String propertyKey;
  final FormFieldValidator<String>? validator;

  const IntTextField({
    super.key,
    this.label,
    required this.propertyKey,
    this.validator,
  });

  @override
  Widget build(BuildContext context) {
    return SkyveTextField(
      propertyKey: propertyKey,
      label: label,
      validator: validator,
      keyboardType: const TextInputType.numberWithOptions(signed: true),
      inputFormatters: [integerFormatter],
    );
  }
}
