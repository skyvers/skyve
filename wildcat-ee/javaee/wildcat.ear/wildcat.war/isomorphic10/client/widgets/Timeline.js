/*
 * Isomorphic SmartClient
 * Version v10.0p_2014-09-10 (2014-09-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */




//>	@class Timeline
// Timeline is a trivial subclass of +link{Calendar} that configures the Calendar with settings
// typical for a standalone timeline view: hides the +link{calendar.dayView, day}, 
// +link{calendar.weekView, week} and +link{calendar.monthView, month} tabs and the 
// +link{calendar.controlsBar, controls bar} by default.
// <P>
// Note that the +link{group:loadingOptionalModules, Calendar module} must be loaded to make 
// use of the Timeline class.
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
