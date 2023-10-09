import 'package:flutter/material.dart';
import 'skyve_textfield.dart';

class SkyvePassword extends StatelessWidget {
  final String label;

  const SkyvePassword({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return SkyveTextField(label: label);
  }
}
