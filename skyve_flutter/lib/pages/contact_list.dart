import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:pluto_grid/pluto_grid.dart';

import '../widget/drawer.dart';

class ContactListPage extends StatefulWidget {
  static const String route = 'ContactList';

  const ContactListPage({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ContactListState();

  void addRow() {
    // widget.
  }
}

class _ContactListState extends State<ContactListPage> {
  late PlutoGridStateManager _gridStateManager;

  @override
  Widget build(BuildContext context) {
    readSampleContactJson().then((value) {
      print('In promise post readSampleContactJson');
      print(value);
      _gridStateManager.appendNewRows(count: 3);
    });

    final grid = _buildGrid();

    return Scaffold(
        appBar: AppBar(title: const Text('Contact List')),
        drawer: buildDrawer(context, ContactListPage.route),
        body: Container(
          child: grid,
        ));
  }

  void addRow(Map<String, dynamic> rowData) {

  }

  PlutoGrid _buildGrid() {
    List<PlutoColumn> columns = <PlutoColumn>[
      PlutoColumn(title: 'Name', field: 'name', type: PlutoColumnType.text()),
      PlutoColumn(
          title: 'Contact Type',
          field: 'contactType',
          type: PlutoColumnType.select(['Person', 'Organisation'])),
      PlutoColumn(
          title: 'Email', field: 'email1', type: PlutoColumnType.text()),
      PlutoColumn(
          title: 'Mobile', field: 'mobile', type: PlutoColumnType.text()),
    ];

    final List<PlutoRow> rows = <PlutoRow>[
      PlutoRow(cells: {
        'name': PlutoCell(value: 'Dave Pipes'),
        'contactType': PlutoCell(value: 'Person'),
        'email1': PlutoCell(value: 'test@example.com'),
        'mobile': PlutoCell(value: null)
      })
    ];

    return PlutoGrid(
      columns: columns,
      rows: rows,
      onLoaded: (PlutoGridOnLoadedEvent event) {
        _gridStateManager = event.stateManager;
      },
    );
  }

  Future<List<Map<String, dynamic>>> readSampleContactJson() async {
    final String response =
        await DefaultAssetBundle.of(context).loadString('./contact_data.json');
    Map<String, dynamic> jsonContent = await json.decode(response);
    List<dynamic> data = jsonContent['response']['data'];

    List<Map<String, dynamic>> md =
        data.map((e) => (e as Map<String, dynamic>)).toList();

    print('Returning ${md.length} contact entries');

    return md;
  }
}
