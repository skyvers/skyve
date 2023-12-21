import 'package:flutter/material.dart';
import 'package:skyve_flutter/widgets/skyve_tab.dart';

class SkyveTabPane extends StatefulWidget {
  final List<SkyveTab> tabs;
  final double height;

  const SkyveTabPane({super.key, required this.tabs, this.height = 0});

  @override
  SkyveTabPaneState createState() => SkyveTabPaneState();
}

class SkyveTabPaneState extends State<SkyveTabPane>
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
              : MediaQuery.of(context).size.height * 0.8,
          margin: const EdgeInsets.only(left: 16.0, right: 16.0),
          child: TabBarView(controller: _tabController, children: widget.tabs),
        )
      ],
    );
  }
}
