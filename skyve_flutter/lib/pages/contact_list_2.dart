import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:pluto_grid/pluto_grid.dart';

import '../widget/drawer.dart';
import '../pages/contact_list.dart';

class ContactListPage2 extends StatefulWidget {
  static const String route = 'ContactList2';

  const ContactListPage2({Key? key}) : super(key: key);

  @override
  State<StatefulWidget> createState() => _ContactListState2();

  void addRow() {
    // widget.
  }
}

class _ContactListState2 extends State<ContactListPage2> {
  late PlutoGridStateManager _gridStateManagerAlternate;

  double rowHeightMobile = 100;
  double rowHeightDesktop = 45;
  double columnWidthP = 550;

  List<PlutoRow> rowsMobile = [];
  List<PlutoRow> fakeFetchedRows = [];

  @override
  void initState() {
    super.initState();
    print("==== init state called");
    loadJson();
  }

  Center getCenter(String value) {
    return Center(
      child: Card(
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: <Widget>[
            ListTile(
              leading: Icon(Icons.album),
              title: Text(value),
              subtitle: Text('Music by Julie Gable. Lyrics by Sidney Stein.'),
            ),
            Row(
              mainAxisAlignment: MainAxisAlignment.end,
              children: <Widget>[
                TextButton(
                  child: const Text('BUY TICKETS'),
                  onPressed: () {/* ... */},
                ),
                const SizedBox(width: 8),
                TextButton(
                  child: const Text('LISTEN'),
                  onPressed: () {/* ... */},
                ),
                const SizedBox(width: 8),
              ],
            ),
          ],
        ),
      ),
    );
  }

  Column getColumnRenderer(PlutoColumnRendererContext rendererContext) {
    return Column(
      children: [
        getTextRow(rendererContext, "bizKey"),
        getTextRow(rendererContext, "name"),
        getTextRow(rendererContext, "contactType"),
        getTextRow(rendererContext, "email1"),
        getTextRow(rendererContext, "mobile")
      ],
    );
  }

  Expanded getTextRow(PlutoColumnRendererContext rendererContext, String v) {
    String textValue = "";
    if (rendererContext.cell.value[v] != null) {
      textValue = rendererContext.cell.value[v].toString();
    }
    return Expanded(
      child: Text(
        // rendererContext
        //     .row.cells[rendererContext.column.field]!.value["name"]
        //     .toString(),
        textValue,
        maxLines: 1,
        overflow: TextOverflow.ellipsis,
      ),
    );
  }

  loadJson() {
    readSampleContactJson().then((value) {
      // List<PlutoRow> rowsMobile = [];

      for (var readRow in value) {
        rowsMobile.add(PlutoRow(cells: {
          'column1': PlutoCell(value: readRow),
          'name': PlutoCell(value: readRow["name"]),
          'contactType': PlutoCell(value: readRow["contactType"]),
          'email1': PlutoCell(value: readRow["email1"]),
          'mobile': PlutoCell(value: readRow["mobile"]),
          'bizKey': PlutoCell(value: readRow["bizKey"])
        }));

        fakeFetchedRows.add(PlutoRow(cells: {
          'column1': PlutoCell(value: readRow),
          'name': PlutoCell(value: readRow["name"]),
          'contactType': PlutoCell(value: readRow["contactType"]),
          'email1': PlutoCell(value: readRow["email1"]),
          'mobile': PlutoCell(value: readRow["mobile"]),
          'bizKey': PlutoCell(value: readRow["bizKey"])
        }));
      }

      if (_gridStateManagerAlternate != null) {
        _gridStateManagerAlternate.appendRows(rowsMobile);
      }

      print(
          "==== data loaded into rowsMobile: " + rowsMobile.length.toString());
    });
  }

  @override
  Widget build(BuildContext context) {
    print("==== build called");
    return LayoutBuilder(builder: (context, constraints) {
      bool mobileView = (constraints.maxWidth <= 800);

      if (mobileView) {
        columnWidthP = constraints.maxWidth;
      }

      Widget body;

      if (constraints.maxWidth < 1000 && constraints.maxWidth > 800) {
        body = getCenter("Some string 1");
      } else {
        PlutoGrid grid = buildAlternateGrid(mobileView);
        print(" ====== number of columns is " + grid.columns.length.toString());
        body = Center(child: grid);
      }

      // if (mobileView) {
      //   body = getCenter("Is mobile view");
      // } else {
      //   body = getCenter("Is not mobile view");
      // }

      print("==== body change");

      return Scaffold(
          appBar: AppBar(title: const Text('Contact List')),
          drawer: buildDrawer(context, ContactListPage2.route),
          body: body);
    });
  }

  void addRow(Map<String, dynamic> rowData) {}

  PlutoGrid buildAlternateGrid(bool mobileView) {
    final List<PlutoRow> rows = <PlutoRow>[];

    List<PlutoColumn> columns = <PlutoColumn>[];

    if (mobileView) {
      columns.addAll([
        PlutoColumn(
          title: 'column1',
          field: 'column1',
          type: PlutoColumnType.text(),
          enableRowDrag: false,
          enableRowChecked: false,
          width: columnWidthP,
          minWidth: 175,
          suppressedAutoSize: false,
          renderer: (rendererContext) {
            return getColumnRenderer(rendererContext);
          },
        ),
      ]);
    } else {
      columns.addAll([
        PlutoColumn(title: 'Name', field: 'name', type: PlutoColumnType.text()),
        PlutoColumn(
            title: 'Contact Type',
            field: 'contactType',
            type: PlutoColumnType.select(['Person', 'Organisation'])),
        PlutoColumn(
            title: 'Email', field: 'email1', type: PlutoColumnType.text()),
        PlutoColumn(
            title: 'Mobile', field: 'mobile', type: PlutoColumnType.text()),
        PlutoColumn(
            title: 'bizKey', field: 'bizKey', type: PlutoColumnType.text()),
      ]);
    }
    print("======columns size " + columns.length.toString());
    return PlutoGrid(
      columns: columns,
      rows: rows,
      onLoaded: (PlutoGridOnLoadedEvent event) {
        print("==== grid state manager retrieved");
        _gridStateManagerAlternate = event.stateManager;
        event.stateManager.setShowColumnFilter(true);
        event.stateManager.appendRows(rowsMobile);
      },
      configuration: PlutoGridConfiguration(
          columnFilter: PlutoGridColumnFilterConfig(
              filters: const [
                ...FilterHelper.defaultFilters,
                // custom filter
                ClassYouImplemented(),
              ],
              resolveDefaultColumnFilter: (column, resolver) {
                print("resolver filter is " + column.field);
                print("resolver filter is " + resolver.toString());

                // if (column.field == 'column1') {
                //   return resolver<ClassYouImplemented>() as PlutoFilterType;
                // } else
                if (column.field == 'text') {
                  return resolver<PlutoFilterTypeContains>() as PlutoFilterType;
                } else if (column.field == 'number') {
                  return resolver<PlutoFilterTypeGreaterThan>()
                      as PlutoFilterType;
                } else if (column.field == 'date') {
                  return resolver<PlutoFilterTypeLessThan>() as PlutoFilterType;
                } else if (column.field == 'select') {
                  return resolver<ClassYouImplemented>() as PlutoFilterType;
                }
                return resolver<PlutoFilterTypeContains>() as PlutoFilterType;
              }),
          style: PlutoGridStyleConfig(
              rowHeight: mobileView ? rowHeightMobile : rowHeightDesktop)),
      createFooter: (s) => PlutoInfinityScrollRows(
        // First call the fetch function to determine whether to load the page.
        // Default is true.
        initialFetch: false,

        // Decide whether sorting will be handled by the server.
        // If false, handle sorting on the client side.
        // Default is true.
        fetchWithSorting: false,

        // Decide whether filtering is handled by the server.
        // If false, handle filtering on the client side.
        // Default is true.
        fetchWithFiltering: false,
        fetch: fetch,
        stateManager: s,
      ),
    );
  }

  Future<List<Map<String, dynamic>>> readSampleContactJson() async {
    final String response =
        await DefaultAssetBundle.of(context).loadString('./contact_data.json');
    Map<String, dynamic> jsonContent = await json.decode(response);
    List<dynamic> data = jsonContent['response']['data'];

    List<Map<String, dynamic>> md =
        data.map((e) => (e as Map<String, dynamic>)).toList();

    print('==== Returning ${md.length} contact entries');

    return md;
  }

  Future<PlutoInfinityScrollRowsResponse> fetch(
    PlutoInfinityScrollRowsRequest request,
  ) async {
    List<PlutoRow> tempList = rowsMobile;
    // examples for pre sort / filtering / etc
    // figuring out is last etc
    // https://github.com/bosskmk/pluto_grid/blob/master/demo/lib/screen/feature/row_infinity_scroll_screen.dart

    Iterable<PlutoRow> fetchedRows = tempList;
    if (request.lastRow == null) {
      fetchedRows = fetchedRows.take(30);
    } else {
      fetchedRows = fetchedRows.skip(1).take(30);
    }

    rowsMobile.addAll(fetchedRows.toList());

    await Future.delayed(const Duration(milliseconds: 500));

    return Future.value(PlutoInfinityScrollRowsResponse(
      isLast: false,
      rows: fetchedRows.toList(),
    ));
  }
}

class ClassYouImplemented implements PlutoFilterType {
  @override
  String get title => 'Custom contains';

  @override
  get compare => ({
        required String? base,
        required String? search,
        required PlutoColumn? column,
      }) {
        var keys = search!.split(',').map((e) => e.toUpperCase()).toList();
        return keys.contains(base!.toUpperCase());
      };

  const ClassYouImplemented();
}
