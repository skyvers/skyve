/*
 * Isomorphic SmartClient
 * Version v9.1p_2014-03-26 (2014-03-26)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */




//>	@class Timeline
// Timeline is a trivial subclass of +link{Calendar} that configures the Calendar with settings
// typical for a standalone timeline view: no other tabs (week, month, day) are shown and the 
// control bar is hidden by default.
//  
// @treeLocation  Client Reference/Calendar
// @visibility external
//<
isc.ClassFactory.defineClass("Timeline", "Calendar");

isc.Timeline.addProperties({

showTimelineView: true,
showDayView: false,
showWeekView: false,
showMonthView: false,
showControlBar: false,

labelColumnWidth: 75,

sizeEventsToGrid: false,
eventDragGap: 0

});
