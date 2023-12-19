import 'package:flutter/material.dart';

class SkyveColourPicker extends StatelessWidget {
  final String label;

  const SkyveColourPicker({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    return const Placeholder(
      fallbackHeight: 50,
      color: Colors.orange,
    );
  }
}
