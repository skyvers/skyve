import 'package:flutter/material.dart';

class SkyveSpinner extends StatelessWidget {
  final String label;

  const SkyveSpinner({super.key, required this.label});

  @override
  Widget build(BuildContext context) {
    // TODO: implement widget
    return const Placeholder(
      fallbackHeight: 50,
      color: Colors.orange,
    );
  }
}
