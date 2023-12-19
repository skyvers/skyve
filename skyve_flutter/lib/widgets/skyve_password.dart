import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/skyve_form.dart';

class SkyvePassword extends StatefulWidget {
  final String label;
  final String propertyKey;

  final FormFieldValidator<String>? validator;

  const SkyvePassword(
      {super.key,
      required this.label,
      required this.propertyKey,
      this.validator});

  @override
  State<StatefulWidget> createState() => SkyvePasswordState();
}

class SkyvePasswordState extends State<SkyvePassword> {
  @override
  Widget build(BuildContext context) {
    var initialVal = SkyveForm.of(context).beanValues[widget.propertyKey] ?? '';

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
      obscureText: true,
    );
  }
}
