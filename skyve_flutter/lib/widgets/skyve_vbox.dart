import 'package:flutter/material.dart';
import 'package:flutter_bootstrap/flutter_bootstrap.dart';

class SkyveVBox extends StatelessWidget {
  final List<Widget> children;

  const SkyveVBox({Key? key, required this.children}) : super(key: key);

  @override
  Widget build(BuildContext context) {
    List<BootstrapRow> rows = List.empty(growable: true);
    for (Widget child in children) {
      List<BootstrapCol> cols = List.empty(growable: true);
      BootstrapCol col =
          BootstrapCol(absoluteSizes: false, sizes: "col-12", child: child);
      cols.add(col);
      BootstrapRow row = BootstrapRow(children: cols);
      rows.add(row);
    }
    return BootstrapContainer(children: rows);
  }
}
