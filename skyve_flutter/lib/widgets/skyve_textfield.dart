import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:skyve_flutter/util/skyve_form.dart';

class SkyveTextField extends StatefulWidget {
  final String? label;
  final String propertyKey;
  final bool obscureText;
  final FormFieldValidator<String>? validator;
  final List<TextInputFormatter>? inputFormatters;
  final TextInputType? keyboardType;

  const SkyveTextField({
    super.key,
    this.label,
    required this.propertyKey,
    this.validator,
    this.obscureText = false,
    this.inputFormatters,
    this.keyboardType,
  });

  @override
  State<StatefulWidget> createState() => SkyveTextFieldState();
}

class SkyveTextFieldState extends State<SkyveTextField> {
  @override
  Widget build(BuildContext context) {
    String initialVal =
        '${SkyveForm.of(context).beanValues[widget.propertyKey] ?? ''}';

    return TextFormField(
      key: Key('${widget.propertyKey}_$initialVal'),
      autovalidateMode: AutovalidateMode.onUserInteraction,
      decoration: InputDecoration(
          border: const OutlineInputBorder(),
          labelText: widget.label,
          errorText: SkyveForm.of(context).serverErrors[widget.propertyKey]),
      initialValue: initialVal,
      onSaved: (newValue) {
        SkyveForm.of(context).beanValues[widget.propertyKey] = newValue ?? '';
      },
      onChanged: (value) {
        setState(() {
          SkyveForm.of(context).removeError(widget.propertyKey);
        });
      },
      validator: widget.validator,
      obscureText: widget.obscureText,
      inputFormatters: widget.inputFormatters,
      keyboardType: widget.keyboardType,
    );
  }
}
