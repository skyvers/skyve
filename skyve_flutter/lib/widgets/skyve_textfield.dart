import 'package:flutter/material.dart';
import 'package:flutter/services.dart';
import 'package:skyve_flutter/util/skyve_flutter_form.dart';
import 'package:skyve_flutter/util/skyve_mixins.dart';
import 'package:skyve_flutter/util/validators.dart';

class SkyveTextField extends StatefulWidget with Sizable {
  final String? label;
  final String propertyKey;
  final bool obscureText;
  final List<Validator>? validators;
  final List<TextInputFormatter>? inputFormatters;
  final TextInputType? keyboardType;

  SkyveTextField({
    super.key,
    required this.propertyKey,
    this.label,
    this.validators = const [],
    this.obscureText = false,
    this.inputFormatters,
    this.keyboardType,
    int? pixelWidth,
  }) {
    this.pixelWidth = pixelWidth;
  }

  @override
  State<StatefulWidget> createState() => _SkyveTextFieldState();
}

class _SkyveTextFieldState extends State<SkyveTextField> {
  @override
  Widget build(BuildContext context) {
    String initialVal =
        '${SkyveFlutterForm.of(context).beanValues[widget.propertyKey] ?? ''}';

    FormFieldValidator<String>? validatorFn =
        _createValidator(widget.validators);

    return TextFormField(
      key: Key('${widget.propertyKey}_$initialVal'),
      autovalidateMode: AutovalidateMode.onUserInteraction,
      decoration: InputDecoration(
          border: const OutlineInputBorder(),
          labelText: widget.label,
          errorText:
              SkyveFlutterForm.of(context).serverErrors[widget.propertyKey]),
      initialValue: initialVal,
      onSaved: (newValue) {
        SkyveFlutterForm.of(context).beanValues[widget.propertyKey] =
            newValue ?? '';
      },
      onChanged: (value) {
        setState(() {
          SkyveFlutterForm.of(context).removeError(widget.propertyKey);
        });
      },
      validator: validatorFn,
      obscureText: widget.obscureText,
      inputFormatters: widget.inputFormatters,
      keyboardType: widget.keyboardType,
    );
  }

  FormFieldValidator<String>? _createValidator(List<Validator>? validators) {
    if (validators == null || validators.isEmpty) {
      return null;
    } else if (validators.length == 1) {
      return validators.first.validate;
    } else {
      return DelegatingValidator(validators).validate;
    }
  }
}
