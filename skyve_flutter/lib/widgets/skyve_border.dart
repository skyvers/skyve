import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';

class SkyveBorder extends Card {
  SkyveBorder({Key? key, String? title, Widget? child})
      : super(
            key: key,
            child: (title == null)
                ? Padding(
                    padding:
                        const EdgeInsets.all(ResponsiveWidth.defaultPadding),
                    child: child)
                : Column(children: [
                    ListTile(
                        title: Text(title,
                            style:
                                const TextStyle(fontWeight: FontWeight.bold))),
                    Padding(
                        padding: const EdgeInsets.only(
                          left: ResponsiveWidth.defaultPadding,
                          right: ResponsiveWidth.defaultPadding,
                          bottom: ResponsiveWidth.defaultPadding,
                        ),
                        child: child!)
                  ]),
            margin: const EdgeInsets.all(0.0));
}
