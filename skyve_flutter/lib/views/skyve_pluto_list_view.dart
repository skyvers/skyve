import 'dart:convert';

import 'package:flutter/material.dart';
import 'package:pluto_grid/pluto_grid.dart';

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
  double rowHeightMobile = 100;
  double rowHeightDesktop = 45;
  double columnWidthP = 550;

  final GlobalKey _widgetKey = GlobalKey();

  List<PlutoRow> rowsMobile = [];
  List<PlutoRow> fakeFetchedRows = [];

  late PlutoGridStateManager _gridStateManager;
  late List<PlutoColumn> columns;

  @override
  void initState() {
    super.initState();
    debugPrint("==== init state called");

    columns = [
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
    ];
    loadJson();
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

  @override
  Widget build(BuildContext context) {
    RenderBox? renderBox =
        _widgetKey.currentContext?.findRenderObject() as RenderBox?;
    Size? size = renderBox?.size; // or _widgetKey.currentContext?.size
    debugPrint('Build Size: ${size?.width}, ${size?.height}');

    bool mobileView = (MediaQuery.of(context).size.width < 800.0);
    debugPrint('build $mobileView');
    return LayoutBuilder(builder: (context, constraints) {
//      bool mobileView = (constraints.maxWidth < 800);
      debugPrint('layout builder ${constraints.maxWidth}');
      if (mobileView) {
        columnWidthP = constraints.maxWidth;
      }

      Widget body;

      // trying to switch the pluto column from single row to multirow
      // however, the grid seems to only redraw properly if I put something in between.
      PlutoGrid grid = _buildGrid(mobileView);
      body = Padding(
          padding: const EdgeInsets.all(80.0), child: Center(child: grid));

      return Scaffold(
          appBar: AppBar(title: const Text('Contact List')),
          body: NotificationListener<SizeChangedLayoutNotification>(
              onNotification: _sizeChanged,
              child: SizeChangedLayoutNotifier(child: body)));
    });
  }

  bool _sizeChanged(SizeChangedLayoutNotification notification) {
    final RenderBox? renderBox =
        _widgetKey.currentContext?.findRenderObject() as RenderBox?;
    final Size? size = renderBox?.size; // or _widgetKey.currentContext?.size
    debugPrint('Changed Size: ${size?.width}, ${size?.height}');

    bool mobile = MediaQuery.of(context).size.width < 800.0;
    debugPrint('size changed $mobile');
    _gridStateManager.hideColumn(columns[0], !mobile, notify: false);
    _gridStateManager.hideColumn(columns[1], mobile, notify: false);
    _gridStateManager.hideColumn(columns[2], mobile, notify: false);
    _gridStateManager.hideColumn(columns[3], mobile, notify: false);
    _gridStateManager.hideColumn(columns[4], mobile, notify: false);
    _gridStateManager.hideColumn(columns[5], mobile, notify: true);
    return true;
  }

  void addRow(Map<String, dynamic> rowData) {}

  PlutoGrid _buildGrid(bool mobileView) {
    final List<PlutoRow> rows = <PlutoRow>[];

    return PlutoGrid(
      key: _widgetKey,
      columns: columns,
      rows: rows,
      onLoaded: (PlutoGridOnLoadedEvent event) {
        debugPrint("==== grid state manager retrieved");
        _gridStateManager = event.stateManager;
        event.stateManager.setShowColumnFilter(true);
        event.stateManager.appendRows(rowsMobile);
      },
      configuration: PlutoGridConfiguration(
          //Column Filtering example
          //https://weblaze.dev/pluto_grid/build/web/#feature/column-filtering
          columnFilter: PlutoGridColumnFilterConfig(
              filters: const [
                ...FilterHelper.defaultFilters,
                // custom filter
                ClassYouImplemented(),
              ],
              resolveDefaultColumnFilter: (column, resolver) {
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
          // Setting the height of the columns. Request to add Dynamic row Height
          //https://github.com/bosskmk/pluto_grid/issues/671
          style: PlutoGridStyleConfig(
              rowHeight: mobileView ? rowHeightMobile : rowHeightDesktop),
          columnSize: const PlutoGridColumnSizeConfig(
              autoSizeMode: PlutoAutoSizeMode.scale)),

      // Infinity Scrolling
      // https://weblaze.dev/pluto_grid/build/web/#feature/row-infinity-scroll
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

        // load data into this so infinity scroll will have data
        // to populate
        fakeFetchedRows.add(PlutoRow(cells: {
          'column1': PlutoCell(value: readRow),
          'name': PlutoCell(value: readRow["name"]),
          'contactType': PlutoCell(value: readRow["contactType"]),
          'email1': PlutoCell(value: readRow["email1"]),
          'mobile': PlutoCell(value: readRow["mobile"]),
          'bizKey': PlutoCell(value: readRow["bizKey"])
        }));
      }

      _gridStateManager.appendRows(rowsMobile);

      debugPrint(
          "==== data loaded into rowsMobile: " + rowsMobile.length.toString());
    });
  }

  Future<List<Map<String, dynamic>>> readSampleContactJson() async {
    final String response =
        await DefaultAssetBundle.of(context).loadString('./contact_data.json');
    Map<String, dynamic> jsonContent = await json.decode(response);
    List<dynamic> data = jsonContent['response']['data'];

    List<Map<String, dynamic>> md =
        data.map((e) => (e as Map<String, dynamic>)).toList();

    debugPrint('==== Returning ${md.length} contact entries');

    return md;
  }

  // Infinity Scrolling
  // https://weblaze.dev/pluto_grid/build/web/#feature/row-infinity-scroll
  Future<PlutoInfinityScrollRowsResponse> fetch(
      PlutoInfinityScrollRowsRequest request) async {
    List<PlutoRow> tempList = rowsMobile;
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

// Custom Filter : Column Filtering example
//https://weblaze.dev/pluto_grid/build/web/#feature/column-filtering
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
