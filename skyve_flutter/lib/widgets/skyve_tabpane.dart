import 'package:flutter/material.dart';
import 'package:skyve_flutter/widgets/skyve_tab.dart';

class SkyveTabPane extends StatefulWidget {
  final List<SkyveTab> tabs;

  const SkyveTabPane({super.key, required this.tabs});

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
    double screenHeight = MediaQuery.of(context).size.height;

    List<Tab> tabButtons = List.generate(widget.tabs.length, (index) {
      return Tab(text: widget.tabs[index].title);
    }, growable: false);

    return Column(
      mainAxisAlignment: MainAxisAlignment.spaceAround,
      children: [
        TabBar(
            controller: _tabController,
            indicatorColor: Colors.orange,
            labelColor: Colors.orange,
            unselectedLabelColor: Colors.black54,
            isScrollable: true,
            tabs: tabButtons),
        Container(
          height: screenHeight * 0.8, // TODO handle this later
          margin: EdgeInsets.only(left: 16.0, right: 16.0),
          child: TabBarView(controller: _tabController, children: widget.tabs),
        )
      ],
    );
  }
}
