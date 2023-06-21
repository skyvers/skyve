import 'package:flutter/material.dart';

class SkyveBorder extends Card {
  SkyveBorder({Key? key, String? title, Widget? child})
      : super(
            key: key,
            child: (title == null)
                ? child
                : Column(children: [ListTile(title: Text(title)), child!]),
            margin: const EdgeInsets.all(8.0));
}
