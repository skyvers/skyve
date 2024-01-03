import 'package:flutter/material.dart';
import 'package:skyve_flutter/util/responsive_grid.dart';
import 'package:skyve_flutter/widgets/skyve_tab.dart';
import 'package:skyve_flutter/widgets/skyve_vbox.dart';
import 'package:skyve_flutter/widgets/skyve_view.dart';

class SkyveTabPane extends StatefulWidget {
  final List<SkyveTab> tabs;
  final double height;

  const SkyveTabPane({super.key, required this.tabs, this.height = 0});

  @override
  State<SkyveTabPane> createState() => _SkyveTabPaneState();
}

class _SkyveTabPaneState extends State<SkyveTabPane>
    with TickerProviderStateMixin {
  late TabController _tabController;

  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: widget.tabs.length, vsync: this);
  }

  @override
  void dispose() {
    super.dispose();
    _tabController.dispose();
  }

  @override
  Widget build(BuildContext context) {
    List<Tab> tabButtons = List.generate(widget.tabs.length, (index) {
      return Tab(text: widget.tabs[index].title);
    }, growable: false);

    List<SingleChildScrollView> tabPanes =
        List.generate(widget.tabs.length, (index) {
      return SingleChildScrollView(
          child: Padding(
              padding: const EdgeInsets.all(ResponsiveWidth.defaultPadding),
              child: SkyveVBox(children: [widget.tabs[index]])));
    }, growable: false);
    return Column(
      children: [
        TabBar(
            controller: _tabController,
            indicatorColor: Colors.orange,
            labelColor: Colors.orange,
            unselectedLabelColor: Colors.black54,
            isScrollable: true,
            tabs: tabButtons),
        Container(
          height: (widget.height > 0)
              ? widget.height
              : SkyveView.screenSize.height * (SkyveView.small ? 0.5 : 0.6),
          margin: const EdgeInsets.only(left: 16.0, right: 16.0),
          child: TabBarView(controller: _tabController, children: tabPanes),
        )
      ],
    );
  }
}
