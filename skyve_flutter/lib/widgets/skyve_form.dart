import 'package:flutter/material.dart';
import 'package:flutter_bootstrap/flutter_bootstrap.dart';
import 'skyve_formcolumn.dart';
import 'skyve_formitem.dart';
import 'skyve_formrow.dart';

class SkyveForm extends StatelessWidget {
  final List<SkyveFormColumn> formCols;
  final List<SkyveFormRow> formRows;

  const SkyveForm({super.key, required this.formCols, required this.formRows});

  @override
  Widget build(BuildContext context) {
    List<Widget> rows = List.empty(growable: true);

    for (SkyveFormRow formRow in formRows) {
      // TODO: replace with column definitions
      List<SkyveFormItem> formItems = formRow.formItems;
      int length = formItems.length;
      int colSize = (12 / length).floor();
      String sizeString = "col-12 col-md-$colSize";

      List<BootstrapCol> bootstrapCols = List.empty(growable: true);
      for (SkyveFormItem formItem in formItems) {
        BootstrapCol col = BootstrapCol(sizes: sizeString, child: formItem);
        bootstrapCols.add(col);
      }

      BootstrapRow bootstrapRow = BootstrapRow(
          decoration: BoxDecoration(
              border: Border.all(color: Colors.transparent, width: 10.0)),
          children: bootstrapCols);

      rows.add(bootstrapRow);
    }

    return BootstrapContainer(
        padding: const EdgeInsets.symmetric(horizontal: 10.0, vertical: 10.0),
        children: rows);
  }
}
