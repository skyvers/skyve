/*
 * Isomorphic SmartClient
 * Version v10.0p_2015-01-04 (2015-01-04)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */

 


// The log functions below will always be defined even with DEBUG> <DEBUG blocks stripped, so that 
// if an end user calls a log function and forgets to mark it with DEBUG, it doesn't result in a
// JS error.

// write special log accessor functions for Class instances so we can call them
isc._logMethods = 
{
    
	logMessage : function (priority, message, category, timestamp) {
        var log = isc.Log;
		if (!log) return;

        //>DEBUG
		
        // if no priority was passed in, use the default
        if (priority == null) priority = log.defaultPriority;
		
		// automatically add a stack trace for error logs
		if (priority <= log.stackTracePriority && this.getStackTrace != null) {
			// skip two levels of the stack to avoid showing the logMessage() invocation itself 
			message += "\nStack trace:\n" + this.getStackTrace(arguments, 2);
		}

		// If a category was not specified, use the name of this class.
		if (!category) category = this.Class;

        var idString = this.ID;
        if (isc.FormItem && isc.isA.FormItem(this) && this.name != null) {
            idString += "[" + this.name + "]";
        }

		// actually do the log.  NOTE: if we have an instance ID, pass it
		log.log(priority, message, category, idString, this, timestamp);

        //<DEBUG
	},

    //> @method class.logDebug()
    // Log a message at "debug" priority
    // <P>
    // A method named log<i>Priority</i> exists for each priority level, on every ISC Class and
    // instance of an ISC Class.  Messages logged on a Class or instance have a default
    // category of the classname.  Messages logged on an instance will also automatically
    // incorporate the instance ID.  General best practice is to call logDebug() et al as
    // "this.logDebug" whenever "this" is an instance, or as "Log.logDebug" otherwise.
    //
    //     @param message    (String)  message to log
    //     @param [category] (String)  category to log in
    //
    // @see Log.echo() for dumping datastructures to the log
    // @see Log.setPriority() for controlling what messages appear in the log
    // @visibility external
    //<
    //> @classMethod class.logDebug()
    // @include method:class.logDebug
    // @visibility external
    //<
    
    // We commonly refer to the classMethod Log.logDebug / logWarn et al
    
    //> @classMethod Log.logDebug()
    // A common usage of +link{classMethod:class.logDebug()} is to call the method directly on
    // the Log class.
    // @include classMethod:class.logDebug()
    // @visibility external
    //<
	logDebug : function (message, category) { return this.logMessage(isc.Log.DEBUG, message, category)},

    //> @method class.logInfo()
    // Log a message at "info" priority
    // 
    //     @param message    (String)  message to log
    //     @param [category] (String)  category to log in
    //
    // @see Log.logDebug() for usage info
    // @visibility external
    //<
    //> @classMethod class.logInfo()
    // @include method:class.logInfo
    // @visibility external
    //<
    
    //> @classMethod Log.logInfo()
    // A common usage of +link{classMethod:class.logInfo()} is to call the method directly on
    // the Log class.
    // @include classMethod:class.logInfo()
    // @visibility external
    //<
	logInfo : function (message, category) { return this.logMessage(isc.Log.INFO, message, category)},

    //> @method class.logWarn()
    // Log a message at "warn" priority
    // 
    //     @param message    (String)  message to log
    //     @param [category] (String)  category to log in
    //
    // @see Log.logDebug() for usage info
    // @visibility external
    //<
    //> @classMethod class.logWarn()
    // @include method:class.logWarn
    // @visibility external
    //<
    
    //> @classMethod Log.logWarn()
    // A common usage of +link{classMethod:class.logWarn()} is to call the method directly on
    // the Log class.
    // @include classMethod:class.logWarn()
    // @visibility external
    //<
	logWarn : function (message, category) { return this.logMessage(isc.Log.WARN, message, category)},

    //> @method class.logError()
    // Log a message at "error" priority
    // 
    //     @param message    (String)  message to log
    //     @param [category] (String)  category to log in
    //
    // @see Log.logDebug() for usage info
    // @visibility external
    //<
    //> @classMethod class.logError()
    // @include method:class.logError
    // @visibility external
    //<
    
    //> @classMethod Log.logError()
    // A common usage of +link{classMethod:class.logError()} is to call the method directly on
    // the Log class.
    // @include classMethod:class.logError()
    // @visibility external
    //<
	logError : function (message, category) { return this.logMessage(isc.Log.ERROR, message, category)},

    //> @method class.logFatal()
    // Log a message at "fatal" priority
    // 
    //     @param message    (String)  message to log
    //     @param [category] (String)  category to log in
    //
    // @see Log.logDebug() for usage info
    // @visibility external
    //<
    //> @classMethod class.logFatal()
    // @include method:class.logFatal
    // @visibility external
    //<
    
    //> @classMethod Log.logFatal()
    // A common usage of +link{classMethod:class.logFatal()} is to call the method directly on
    // the Log class.
    // @include classMethod:class.logFatal()
    // @visibility external
    //<
	logFatal : function (message, category) { return this.logMessage(isc.Log.FATAL, message, category)},

    //> @method class.logIsEnabledFor()
    // Check whether a message logged at the given priority would be visible in the log.
    // <P>
    // As with logDebug, category is defaulted to the current className.  Use this method to avoid
    // putting together expensive log messages if they will never appear in the log.
    //
    //     @param priority   (LogPriority)  priority level
    //     @param [category] (String)            category to log in
    // @visibility external
    //<
    //> @classMethod class.logIsEnabledFor()
    // @include method:class.logIsEnabledFor
    // @visibility external
    //<
	logIsEnabledFor : function (priority, category) {
        return (isc.Log.isEnabledFor && 
                isc.Log.isEnabledFor((category ? category : this.Class), priority, this))
    },

    //> @method class.logIsDebugEnabled()
    // Check whether a message logged at "debug" priority would be visible in the log.
    // <P>
    // As with logDebug, category is defaulted to the current className.  Use this method to avoid
    // putting together expensive log messages if they will never appear in the log.
    //
    //     @param [category] (String)            category to log in
    // @visibility external
    //<
    //> @classMethod class.logIsDebugEnabled()
    // @include method:class.logIsDebugEnabled
    // @visibility external
    //<
	logIsDebugEnabled : function (category) { return this.logIsEnabledFor(isc.Log.DEBUG, category) },
    
    //> @method class.logIsInfoEnabled()
    // Check whether a message logged at "info" priority would be visible in the log.
    // <P>
    // As with logDebug, category is defaulted to the current className.  Use this method to avoid
    // putting together expensive log messages if they will never appear in the log.
    //
    //     @param [category] (String)            category to log in
    // @visibility external
    //<
    //> @classMethod class.logIsInfoEnabled()
    // @include method:class.logIsInfoEnabled
    // @visibility external
    //<
	logIsInfoEnabled : function (category) {    return this.logIsEnabledFor(isc.Log.INFO, category) },
    
    //> @method class.logIsWarnEnabled()
    // Check whether a message logged at "warn" priority would be visible in the log.
    // <P>
    // As with logDebug, category is defaulted to the current className.  Use this method to avoid
    // putting together expensive log messages if they will never appear in the log.
    //
    //     @param [category] (String)            category to log in
    // @visibility external
    //<
    //> @classMethod class.logIsWarnEnabled()
    // @include method:class.logIsWarnEnabled
    // @visibility external
    //<
	logIsWarnEnabled : function (category) {    return this.logIsEnabledFor(isc.Log.WARN, category) },

    //> @method class.logIsErrorEnabled()
    // Check whether a message logged at "error" priority would be visible in the log.
    // <P>
    // As with logDebug, category is defaulted to the current className.  Use this method to avoid
    // putting together expensive log messages if they will never appear in the log.
    //
    //     @param [category] (String)            category to log in
    // @visibility external
    //<
    //> @classMethod class.logIsErrorEnabled()
    // @include method:class.logIsErrorEnabled
    // @visibility external
    //<
	logIsErrorEnabled : function (category) {    return this.logIsEnabledFor(isc.Log.ERROR, category) },

    // Methods to update the log priority directly on objects
    
    //> @method class.setLogPriority()
    // Set the priority of messages that will be visible for some log category, when logged on
    // this Class or Instance object.<br>
    // If called with no category, this priority will be applied to every logged message on this
    // object<br>
    // To set the visible log priority for some category across the entire page, use
    // <code>isc.Log.setPriority()</code> instead.
    // @param category (string) Category for which the log priority will be updated. If not
    //                          all logs on this canvas will be logged at the priority passed in.
    // @param priority (LogPriority) priority level
    // @see Log.setPriority()
    // @visibility external
    //<
    //> @classMethod class.setLogPriority()
    // @include method:class.setLogPriority
    // @visibility external
    //<
    
    //> @classMethod Log.setLogPriority()
    // A common usage of +link{classMethod:class.setLogPriority()} is to call the method
    // directly on the Log class.
    // @include classMethod:class.setLogPriority()
    // @visibility external
    //<
    setLogPriority : function (category, priority) {
        isc.Log.setPriority(category, priority, this);
    },
    
    //> @method class.setDefaultLogPriority()
    // Set the default priority of logging for messages logged on this Class or Instance object.
    // All categories for which there is no explicit, instance level logging priority set will
    // log at this level on this object.<br>  
    // To set the default visible log priority across the entire page, use
    // <code>isc.Log.setDefaultPriority()</code> instead.
    // @param category (string) Category for which the log priority will be updated. If not
    //                          all logs on this canvas will be logged at the priority passed in.
    // @param priority (LogPriority) priority level
    // @see Log.setPriority()
    // @visibility external
    //<
    //> @classMethod class.setDefaultLogPriority()
    // @include method:class.setDefaultLogPriority
    // @visibility external
    //<
    
    //> @classMethod Log.setDefaultLogPriority()
    // A common usage of +link{classMethod:class.setDefaultLogPriority()} is to call the
    // method directly on the Log class.
    // @include classMethod:class.setDefaultLogPriority()
    // @visibility external
    //<
    setDefaultLogPriority : function (priority) {
        isc.Log.setDefaultPriority(priority, this);
    },
    
    //> @method class.getDefaultLogPriority()
    // Retrieves the default priority of messages for this class or instance.
    // @return (LogPriority) default priority for logging messages on this object.
    // @visibility external
    //<
    //> @classMethod class.getDefaultLogPriority()
    // @include method:class.getDefaultLogPriority
    // @visibility external
    //<
    
    //> @classMethod Log.getDefaultLogPriority()
    // A common usage of +link{classMethod:class.getDefaultLogPriority()} is to call the
    // method directly on the Log class.
    // @include classMethod:class.getDefaultLogPriority()
    // @visibility external
    //<
    getDefaultLogPriority : function () {
        return isc.Log.getDefaultPriority(this);
    },

    //> @method class.clearLogPriority()
    // Clear this object's priority setting for a particular category, so that the category's 
    // effective priority returns to the specified priority for this category at the Log level
    // (or <code>Log.defaultPriority</code> if not set).<br>
    // To clear the Page-level priority setting for this log category use 
    // <code>isc.Log.clearPriority()</code> instead.
    //
    // @param category   (String) Category name. If not specified, all logging on this object
    //                              will revert to default priority settings.
    // @visibility external
    // @see Log.clearPriority()
    //<
    //> @classMethod class.clearLogPriority()
    // @include method:class.clearLogPriority
    // @visibility external
    //<
    clearLogPriority : function (category) {
        isc.Log.clearPriority(category, this);
    }    
    
};

// add the methods to Class object prototype and to the Class instance prototype
isc.Class.addMethods(isc._logMethods);
isc.Class.addClassMethods(isc._logMethods);



//> @groupDef serverLogging
// <h3>Default logging</h3>
// <p>
// SmartClient's server-side classes have extensive built-in diagnostics which are output via
// the +externalLink{http://logging.apache.org/log4j/1.2/,Apache Log4j library} (see below for
// other alternatives).
// <p>
// At startup, SmartClient will automatically load the file <code>log4j.isc.config.xml</code>
// from the classpath and use it to configure Log4j.  <code>log4j.isc.config.xml</code> is in
// Log4j's standard 
// +externalLink{http://wiki.apache.org/logging-log4j/Log4jXmlFormat,XML configuration format},
// and sets default log threshold levels for various subsystems to produce output that is
// generally appropriate for both development and production systems.  Various SmartClient
// documentation may encourage you to enable certain diagnostic logs using this file when
// troubleshooting specific problems.
// <p>
// <h3>Server Logs tab (SmartClient Developer Console)</h3>
// <p>
// The Server Logs tab of the +link{group:debugging,SmartClient Developer Console} provides the
// ability to view the most recent 500 log entries, and change log threshold levels dynamically 
// at runtime.
// <p>
// <h3>Redirecting logging to other frameworks</h3>
// <p>
// SmartClient server logging can alternatively use the Simple Logging Facade for Java (slf4j),
// which allows logs to be sent to a variety of different logging frameworks that support
// slf4j.
// <p>
// To send all logging to slf4j, the <code>iscUseSlf4j</code> VM argument must be set to true
// on the command line, like this:
// <pre>
// -DiscUseSlf4j=true
// </pre>
// If slf4j is used and the underlying log system is still Log4j, SmartClient will still
// configure Log4j using <code>log4j.isc.config.xml</code> as describe above <i>unless</i> you
// pass an additional command line argument to prevent this:
// <pre>
// -DiscUseLog4jConfig=false
// </pre>
// If slf4j is used with any other logging system, SmartClient will not attempt to apply
// configuration - see the
// +externalLink{http://www.slf4j.org/manual.html,SLF4J user manual} for details on 
// how to configure slf4j.
// <p>
// Note that the features of the "Server Logs" tab will <b>not</b> be available if using slf4j,
// even if Log4j is also used.
// <p>
// <h3>Configure custom log4j loggers</h3>
// <p>
// If log4j is used and custom loggers are configured in <code>log4j.isc.config.xml</code> 
// file, use <code>DataTools.getLoggerRespository()</code> method to access them on server side, 
// like this:
// <pre>
// DataTools.getLoggerRepository().getLogger(CustomClass.class.getName());
// </pre>
//
// @title Server logging
//<

//>	@groupDef debug
// Support for debugging and logging
//<

//>	@class Log
// A logging system similar to the Java log4j package: messages are logged with a "category" and
// "priority", and developers can dynamically set which log messages are being displayed.
// <P>
// 5 log priorities are available, with the following general meaning:
// <ul>
// <li> "debug": diagnostic info which is only likely to be understood by a developer with
// source access, or would occur too frequently for normal usage
// <li> "info": reports of significant events in the normal operation of the subsystem
// <li> "warn": some kind of problem is likely to occur, an API appears is apparently being
// misused or will yield a partial or very slow result
// <li> "error": a definite error has occurred which may be recoverable
// <li> "fatal": total failure with no possibility of recovery
// </ul>
// <P>
// Log categories do not need to be declared in advance - you can simply make up a category name and
// start logging to it, and control whether that category's messages will be displayed via
// <code>setPriority()</code>.
// <P>
// <b>NOTE:</b> to open the Developer Console in any page that loads ISC, type
// javascript:isc.Log.show() in the URL bar - this URL is bookmarkable.  
// <P>
// The Developer Console should <b>always</b> be open while developing any ISC-enabled application,
// because ISC logs many important errors and warnings to the Developer Console.
// <P>
// In Internet Explorer, the Developer Console is able to log a stack trace for every JS error,
// including errors that occur in non-ISC code.
// <P>
// NOTE: if you have the Microsoft JavaScript Debugger installed, ISC will be unable to log stack
// traces on JS errors until you go to Tools->Internet Options->Advanced Tab and check "Disable
// script debugging".  The ability to see stack traces in the Developer Console is generally much
// more useful for debugging ISC-based applications than the generic Javascript Debugging
// facilities.
//
// @treeLocation Client Reference/System
// @group debug
//
// @see Log.setPriority()
//
//  @visibility external
//<
isc.ClassFactory.defineClass("Log");

//> @groupDef debugging
// <smartgwt>
// <h4>Development and Super Dev Modes</h4>
// GWT
// +externalLink{http://www.gwtproject.org/doc/latest/DevGuideCompilingAndDebugging.html#dev_mode,Development Mode},
// which runs your Java code in an actual Java VM attached to the browser via a browser plugin,
// is being phased out of the GWT project in favor of
// +externalLink{http://www.gwtproject.org/articles/superdevmode.html,Super Dev Mode}, which
// translates your Java code into JavaScript before running it, even when debugging.
// <p>
// In Super Dev Mode,
// +externalLink{https://developer.chrome.com/devtools/docs/javascript-debugging#source-maps,source maps} 
// provided by a GWT "code server" Java app allow breakpoints to be placed at chosen locations
// within the Java source code.  Running Super Dev Mode consists of two main steps:
// <ul>
// <li> Adding a bit of additional configuration to your GWT project file (gwt.xml) and rebuilding
// <li> Running the GWT Code Server Java App (either from the command line or from Eclipse)
// </ul><p>
// Once these two steps have been completed, you can run the web application through Eclipse,
// or deploy it manually to an existing web server.  The code server can then be invoked
// by a browser bookmark and the source maps for your project navigated to place breakpoints
// where needed.
// <P>
// Note that Google Chrome is recommended as the primary browser for use with Super Dev Mode,
// because while the built-in Firefox debugger supports source maps, it doesn't appear to stop
// at breakpoints (as of Firefox 31).  The separate "Firebug" debugger does not at this time
// support source maps at all, and will only gain such support when it is
// +externalLink{https://code.google.com/p/fbug/issues/detail?id=5765,integrated directly into the Firefox debugger}.
// <p>
// <i>Refer to +link{superDevModeTroubleshooting, Troubleshooting Super Dev Mode} for more
// detailed help running Super Dev Mode.</i>
// <P>
// Classic Development Mode can now only be used with older versions of Firefox, such as 
// +externalLink{https://ftp.mozilla.org/pub/mozilla.org/firefox/releases/latest-24.0esr/,Firefox 24ESR}
// (which is only supported until 10/14/2014).  There are no plans to re-add support in current
// versions of Firefox.  Chrome's Development Mode also cannot be used because of
// +externalLink{http://forums.smartclient.com/showthread.php?t=8159#aChrome,limitations of Chrome} 
// that break core GWT functionality.
// <P>
// <h4>Stack Details</h4>
// When not using one of the GWT development modes described above, the stack trace presented in
// the SmartClient Developer Console will consist of obfuscated method names and provide little
// helpful information.  Useful unobfuscated traces may be obtained by adding "-optimize 0" and
// "-style DETAILED" or "-style PRETTY" to the gwtc ant target in the build.xml file for your
// project.  A more useful stack trace in certain cases can sometimes be obtained by activating
// GWT "stack emulation".  To do this, set the property <code>compiler.stackMode</code> to
// "emulated" and <code>compiler.emulatedStack.recordLineNumbers</code> to "true" in your
// gwt.xml project file.  (Note that enabling stack emulation will increase the size of the
// JavaScript files generated by GWT.)  Examples of both of these enhancements are provided 
// as commented-out lines in the appropriate files of the BuiltInDS SGWT sample 
// <P>
// Note that due to how GWT compiles Java code into JavaScript, the top of the stack may
// actually go deeper than expected in the visible trace when a crash in the Java code occurs.
// So for example if you try to invoke a method on a null object like:
// <pre>
//     ListGrid listGrid = null;
//     listGrid.setWidth100();</pre>
// then the actual Development Console error will look something like:
// <pre>
// 23:11:44.461:WARN:Log:TypeError: this$static is null
// Stack from error.stack:
//     $isCreated_4() @ showcase/239D5C0DDE9A2775E194CC3519D90866.cache.html:22080:7
//     $setAttribute_20() @ showcase/239D5C0DDE9A2775E194CC3519D90866.cache.html:22132:3
//     $setWidth_3() @ showcase/239D5C0DDE9A2775E194CC3519D90866.cache.html:23349:3
//     $setWidth100() @ showcase/239D5C0DDE9A2775E194CC3519D90866.cache.html:23353:3
//         :</pre>
// (where -optimize 0 and -style PRETTY have been added to build.xml as mentioned above).
// This is because the JavaScript code emitted by GWT for <code>setWidth100()</code>,
// <code> setWidth()</code>, and <code>setAttribute()</code> never actually attempts
// to access the widget (<code>this$static</code>) until execution reaches 
// <code>BaseWidget.isCreated()</code>.
// <P>
// </smartgwt>
// <h4>Built-in Diagnostics</h4>
// <P>
// The SmartClient Developer Console is a suite of development tools implemented in SmartClient itself. 
// The Console runs in its own browser window, parallel to your running application, so it is always 
// available in every browser, and in every deployment environment.
// <P>
// The Developer Console can be opened by calling <code>isc.showConsole()</code> on any page in which 
// SmartClient has been loaded. You can create a bookmark in your browser to quickly show the Console on 
// any SmartClient application, without any changes to the application code:
// <P>
// 1. Create a new bookmark in your browser.<BR>
// 2. Enter url "javascript:isc.showConsole()".<BR>
// 3. Label the bookmark as "Show Console".<BR>
// 4. Consider adding this to the Bookmarks Toolbar. This allows one-click access to the Console 
// from any SmartClient application.
// <P>
// Note: For most browsers you can evaluate javascript directly from the browser URL bar by entering 
// <code>javascript:<i>string to evaluate</i></code> directly in the URL bar, so setting up a bookmark 
// is not strictly necessary. For Firefox 6 and above, this feature has been disallowed, but the bookmark 
// approach will still work. Alternatively developers could use 
// +externalLink{http://blog.mozilla.com/devtools/2011/08/15/introducing-scratchpad/,Firefox Scratchpad} 
// to launch the console.
// <P>
// Basic information on the features of the Developer Console can be found in the QuickStart
// Guide.  For information about the "RPC" tab of the Developer Console and the request 
// profiling information it can provide, see 
// +link{groupDef:devConsoleRPCTab,the Developer Console RPC tab}.  The Develper Console also
// supports debugging of remote pages (very useful for mobile devices) - see
// +link{groupDef:remoteDebugging} for more information.  The remainder of this 
// topic focuses on use of the log system and related debugging facilities.
// <P>
// The Developer Console contains a "Results" pane that displays a list of diagnostic
// messages logged by the SmartClient framework. The "Logging Preferences" menu lets you
// enable and disable SmartClient's built-in diagnostics in several categories. Because
// important diagnostic messages may be logged at any time, you should have the Developer
// Console open whenever you are working with SmartClient (and you should bookmark the
// "javascript:" expression above to make this easier).
// <P>
// Log messages are of the format:
// <P>
// &nbsp;&nbsp;&nbsp;<i>timestamp</i>:<i>priority</i>:<i>category</i>:<i>message</i>
// <P>
// For example, the following log message:
// <pre>
//     11:59:25:806:INFO:Page:Page loading complete.</pre>
// Occurred at 11:59:25 local time and 806 milliseconds.  It's priority was <code>INFO</code>,
// it occurred in the category <i>Page</i>, and the message is "Page loading complete.".
// <P>
// Each logging <i>category</i> has a <i>priority</i> associated with it.  If a message's
// priority is lower than the current priority for the category it is logged in, the
// message will be suppressed (will not appear in the "Results" pane).  
// <p>
// It is critical to be familiar with the diagnostic categories built-in to SmartClient - 
// you will use them in most debugging sessions.  Open the Logging Preferences menu and select
// "More.." to see a list of diagnostic log categories.   Hover over each category name to
// see a description of what kind of messages are logged in the category.  
// <P>
// <h4>Debugging JavaScript Errors</h4>
// <P>
// Javascript errors will typically be reported in the Developer Console. Wherever possible a stack 
// trace will be included which can help determine the cause of the error.
// In addition to this, recent versions of the Firefox browser (versions 6.0 and above) ship with some 
// useful development tools including the Error Console for reporting errors. We also recommend Console2 
// and Firebug for debugging in Firefox.
// <P>
// In Internet Explorer, when JS errors occur, SmartClient is able to report full stack traces
// in the Developer Console.  This can be invaluable when your code triggers a JS error
// in the SmartClient libraries themselves, or when it is unclear how your code is being
// called.  Stack traces from Internet Explorer should <i>always</i> be included in issue
// reports sent to Isomorphic Software, if at all possible. 
// <P>
// <h4>Inspecting application state</h4>
// <P>
// The "Evaluate JS Expression" area of the Results Pane in the Developer Console can be used
// to inspect the current state of a SmartClient application.  Any SmartClient or browser
// built-in API can be called from the "Evaluate JS Expression" area, and the results will
// be intelligently summarized (via +link{Log.echo()}).  For example, simply typing a
// component's ID and pressing the "Eval JS" button will give you a dump of it's current
// property values.  
// <P>
// Many, many SmartClient APIs can be usefully called while troubleshooting, eg, 
// +link{listGrid.data} is a +link{ResultSet} when a grid is DataBound and 
// +link{resultSet.get()} can be called to inspect the current values on records.  In addition,
// new application code can be tried out, for example, you might repeatedly instantiate a new
// component, trying variants on the properties you could give it.
// <P>
// <b>Inspecting transient application state with logs</b>
// <P>
// Transient state, such as the values of local variables in a method that is crashing, can be
// sent to the Developer Console via using the <smartclient>+link{Log}</smartclient>
// <smartgwt>+externalLink{../util/SC.html,SC}</smartgwt> class.  For example, to dump the
// value of <smartclient>the local variable</smartclient><smartgwt>a local instance of 
// +link{DataClass}</smartgwt> "request":
// <smartclient>
// <pre>
//     isc.logWarn("request is: " + isc.echo(request));</pre>
// </smartclient><smartgwt>
// <pre>
//     SC.logWarn("request is: " + SC.logEcho(request.getJsObj()));</pre>
// </smartgwt>
// It's a good idea to dump the values of local variables in any method that is crashing or
// behaving unexpectedly.
// <P>
// Note the use of <smartclient>+link{classMethod:isc.logWarn,logWarn()}</smartclient>
// <smartgwt>+externalLink{../util/SC.html#logWarn(java.lang.String),SC.logWarn()}</smartgwt>
// above: in typical debugging sessions, it's best to simply use the <code>logWarn</code> method
// to output diagnostics to ensure your message will not be suppressed by log priority settings.
// <P>
// <smartclient>
// NOTE: never use the native <code>alert()</code> method to output diagnostics.  Among other
// issues, <code>alert()</code> can affect timing, masking or altering the behavior you were
// trying to debug.  SmartClient's logging system doesn't suffer from these problems and
// provides much more control.
// <P>
// </smartclient>
// <h4>Issue Reports</h4>
// <P>
// If you believe you've discovered a bug in SmartClient or you are having trouble using
// SmartClient APIs, you can report it in 
// +externalLink{http://forums.smartclient.com/,the SmartClient Forums}.
// <P>
// <b>How quickly your issue is resolved is entirely up to you</b>.  If you follow the steps
// below and submit an appropriate issue report, you will generally receive a rapid solution
// from Isomorphic Support, regardless of what support level you have, because Isomorphic
// aggressively corrects bugs and legitimate usage issues.  If you skip steps you are likely to
// be directed back to this document and asked to submit a more complete issue report.
// <P>
// Before reporting an issue, ensure that you:
// <ul>
// <li> Have read the +docTreeLink{QuickStartGuide,QuickStart Guide} cover to
// cover.  Later chapters cover more advanced topics and provide links to further examples and
// reference.
// <li> Have searched the 
// <smartclient>
// +docTreeLink{FeatureExplorer,Feature Explorer}
// </smartclient><smartgwt>
// SGWT Showcase from your installation (e.g.
// +externalLink{http://www.smartclient.com/smartgwt/showcase/, SGWT LGPL Showcase} /
// +externalLink{http://www.smartclient.com/smartgwtee/showcase/, SGWT EE Showcase})
// </smartgwt>
// for examples that show what you are trying to do
// <li> Have searched this reference, trying multiple searches using different, common and
// related terms for what you are trying to do (eg for search, try "search", "filter",
// "criteria", "find", "match", etc)
// <li> Have searched the public +externalLink{http://forums.smartclient.com,forums}
// </ul>
// Always include:
// <ul>
// <li> A description of what you are trying to accomplish <b>from an end user's perspective</b>.
// The best answers often point out a simpler approach.
// <li> The browser(s), operating system(s) and SmartClient version(s) you experience the error
// on (SmartClient version is available in the lower-left handle corner of the Developer
// Console)
// </ul>
// Then, include <b>either</b> a standalone test case (see below), <b>or</b>:
// <ul>
// <li> For JS errors, Stack traces from Firebug (for Firefox) or the Developer Console (for
// IE), as covered under "Debugging JavaScript Errors" above
// <li> What server platform and +link{group:clientServerIntegration,databinding approach} you
// are using, if applicable
// <li> contents of the SmartClient Developer Console "Log messages" area, with appropriate
// diagnostic categories set the DEBUG or INFO level (see "Built-in Diagnostics" above)
// <li> for any problem involving server contact, the complete server-side log for the request
// that fails or produces unexpected results
// <smartgwt>
// <li> if using GWT Development Mode, any exceptions reported from Java in Eclipse
// </smartgwt>
// <li> Results of calling <code>echo()</code> on local variables or other application
// state you think is relevant (see "Inspecting Application State" above)
// <li> sample code and sample data
// </ul>
// <b>Preparing a standalone test case</b>
// <P>
// A standalone test case is one of:
// <smartclient>
// <ol>
// <li> a chunk of JavaScript code that can be executed from the "Eval JS" area of the
// Developer Console on some specified page within the unmodified SmartClient SDK,
// demonstrating your issue
// <li> an .html or .jsp file that can be dropped at a specified location into an unmodified
// SmartClient SDK and will run without changes, demonstrating your issue.
// <li> a .zip file that includes a standalone .html/.jsp file  as above, as well as
// dependencies required to make the test case runnable, such as XML datasets
// </ol>
// </smartclient><smartgwt><ol>
// <li>a modified version of one of our sample projects, such as the BuiltInDS sample
// with BuiltInDS.java changed to demonstrate your issue
// <li>a modified version of any one of our SGWT Showcase samples (e.g. GridSortSample);
// choosing one that requires only a slight modification to demonstrate your issue is best
// </ol>
// Note: Ideally this results in a single Java file you provide to us if possible.
// </smartgwt>
// <P>
// Submitting a standalone test case removes any ambiguity as to whether there is a bug in
// SmartClient or a bug in your code, and eliminates the possibility of Isomorphic Support
// responding with a "works for me" result due to incomplete information.  Issues with verified
// test cases are routed directly to the engineer that authored the relevant SmartClient
// subsystem, often as the new highest priority task.  In addition, the process of preparing a
// test case very often allows you to solve the issue yourself, if the underlying issue is not
// actually a framework bug.
// <P>
// There are two approaches to test case preparation:
// <ol>
// <li> Add code to an existing <smartclient>SmartClient example</smartclient>
// <smartgwt>SGWT Showcase sample</smartgwt> until you can reproduce the problem
// <li> Remove code from your application until it minimally shows the problem and runs standalone
// </ol>
// <P>
// For approach #1, find the nearest match to your use case in the 
// <smartclient>
// +docTreeLink{FeatureExplorer} examples or in the other examples accessible from the Examples
// folder of the SDK, then try to minimally modify that example to demonstrate your issue.
// Feature Explorer examples are a particularly good starting point because you can simply copy
// the code from the Feature Explorer to the Eval JS area of the Developer Console and begin
// changing it, and if successful this yields a type #1 test case, 
// </smartclient><smartgwt>
// SGWT Showcase samples or in the sample projects included in the SGWT download package, then
// try to minimally modify that sample to demonstrate your issue.  This should yield a 
// #1 test case,
// </smartgwt>
// the easiest for you to submit and most efficient for Isomorphic to work with.
// <P>
// For approach #2,
// <ol>
// <li> If a server is involved in initial page generation (eg a .jsp file), in most cases you
// can eliminate many server dependencies <b>and</b> create an easily modifiable starting point
// by using the browser's "View Source" feature to save a copy of the generated HTML output as
// an .html file in the same directory as the .jsp file that generated it.  Such a file will
// generally continue to function (all relative paths are still correct), and can be modified
// freely without the need to later revert changes to a .jsp.
// <li> Eliminate any code that isn't involved in the interaction.  Keep running the test case
// as you eliminate code to ensure you are still seeing the issue (you may solve it this way,
// or find key preconditions that you can report to Isomorphic)
// <li> For any issue that isn't cosmetic, revert to a default SmartClient skin
// <li> For any necessary RPC/DataSource interactions, spoof the interaction with one of these
// approaches:
// <ul>
// <li> switch any DataSources to one of the sample DataSources from the <smartclient>SDK
// (eg "supplyItem")</smartclient><smartgwt>SGWT Showcase (e.g. ItemSupplyXmlDS)</smartgwt>
// if your issue can still be reproduced in this case.
// <li> create a small sample dataset in <smartclient>JavaScript directly in the .html file,
// </smartclient><smartgwt>Java directly in the Java sample file</smartgwt> and use a
// +link{dataSource.clientOnly,clientOnly DataSource} with that dataset.
// <li> capture server responses verbatim by setting the RPCManager log category to DEBUG, save
// the responses as flat files, and set +link{dataSource.dataURL} to point at them.
// <li> for RPCs, instead of calling the RPCManager, directly call your own callback function,
// passing a spoofed RPCResponse that includes just the fields your code depends upon
// </ul>
// <smartclient>
// <li> Finally, move your .html file into the stock SmartClient SDK</smartclient>
// <smartgwt>
// <li> Finally, move your Java file into one of the stock SGWT sample projects,
// such as the BuiltInDS sample,</smartgwt>
// along with any remaining dependencies and verify the problem can still be reproduced
// </ol>
// Having prepared the test case, combine it with the other required issue report information
// covered above, and submit it to the +externalLink{http://forums.smartclient.com/,forums},
// or, if you have Enterprise Support, at the
// +externalLink{http://support.isomorphic.com/,Customer Support Extranet}.
// <P>
// <h4>Using the Debug Modules (Advanced)</h4>
// <P>
// See +link{group:debugModules,Using the Debug Modules}.
// <P>
// <h4>Adding your own diagnostic categories</h4>
// <P>
// Calling <code>logWarn()</code> is fine for a log statement you plan to delete at the end of
// the debugging session.  However, many log statements have lasting value if you could enable
// or disable them only when you need the relevant diagnostics, like SmartClient's built-in
// diagnostic categories.  To do this, pick a priority level less than <code>WARN</code>
// (<code>INFO</code> or <code>DEBUG</code>), and call the corresponding method on the 
// <smartclient>Log</smartclient><smartgwt>SC</smartgwt> class (<code>logInfo()</code> or
// <code>logDebug()</code>), passing the category name as a second parameter.  For example:
// <smartclient>
// <pre>
//     isc.Log.logInfo("first record is: " + isc.Log.echo(myGrid.data.get(0)),
//                     "myGridLoading");
// </pre>
// </smartclient><smartgwt>
// <pre>
//     Sc.logInfo("first record is: " + 
//                SC.logEcho(myGrid.getDataAsRecordList().get(0).getJsObj()),
//                "myGridLoading");
// </pre>
// </smartgwt>
// This message will no longer appear in the Results Pane by default, because its priority
// (<code>INFO</code>) is less than the default of <code>WARN</code>.  To see this message,
// open the Logging Preferences menu and pick "More..", then click the "Add" button, enter
// "myGridLoading" as the category name and set the priority to <code>INFO</code>.  The message
// will now appear next time it is logged.
// <P>
// Now you have a custom log category that you and other developers can use to debug your
// application, subsystem by subsystem.  These diagnostics will be available to you both in
// development and production environments.
// <P>
// As with SmartClient's built-in diagnostics, you may choose to log certain messages in your
// custom category at the <code>DEBUG</code> level and a lesser number of messages at the
// <code>INFO</code> level, to create different depths of diagnostic output.
// <P>
// <smartclient>
// <h4>Logging refinements</h4>
// <P>
// The core log methods (<code>logDebug()</code>, <code>logInfo()</code>,
// <code>logWarn()</code>) and the "echo" facilities (<code>echo()</code> and
// <code>echoAll()</code>) are available on every SmartClient component and Class.  Hence,
// in many cases, the special JavaScript value "this" will refer to an object that supports
// <code>logWarn()</code> et al.  For example:
// <pre>
//     isc.Canvas.create({
//        ID:"canvasExample",
//        contents:"Hello World!",
//        click:"this.logWarn('the Canvas is: ' + this.echo(this))"
//     });
// </pre>
// The special value "this" is not always set to a SmartClient component, for example, in some
// kinds of callbacks (eg +link{ListGrid.fetchData(),fetchData()}).  When in doubt, use these
// methods via the Log class as <code>isc.Log.logWarn()</code>.
// <P>
// </smartclient>
// <b>Logging performance</b>
// <P>
// Because the log message is actually formed <i>before</i> the call to the log system, logs
// that are suppressed can still carry a performance penalty.  This is particularly true of
// logs that output a lot of data or occur frequently.  To avoid this penalty, you can check in
// advance whether a message will be suppressed using
// <smartclient> 
// +link{classMethod:Class.logIsDebugEnabled(),isc.Log.logIsDebugEnabled()} and
// +link{classMethod:Class.logIsInfoEnabled(),isc.Log.logIsInfoEnabled()}.  For example:
// <pre>
//     if (isc.Log.logIsInfoEnabled("myGridLoading")) {
//        isc.Log.logInfo("first record is: " + isc.Log.echo(myGrid.data.get(0)),
//                        "myGridLoading");
//     }
// </pre>
// </smartclient><smartgwt>
// +externalLink{../util/SC.html#logIsDebugEnabled(java.lang.String),SC.logIsDebugEnabled()} and
// +externalLink{../util/SC.html#logIsInfoEnabled(java.lang.String),SC.logIsInfoEnabled()}.
// For example:
// <pre>
//     if (SC.logIsInfoEnabled("myGridLoading")) {
//         SC.logInfo("first record is: " +
//                    SC.logEcho(myGrid.getDataAsRecordList().get(0).getJsObj()),
//                    "myGridLoading");
//     }
// </pre>
// </smartgwt>
// Generally, it is only important to do this for logs that will occur multiple times during a
// given user interaction (eg a mousedown or keypress) and/or that call <code>echo()</code> on
// objects with many properties.
//
// @title Debugging
// @treeLocation Concepts
// @see serverLogging
// @see remoteDebugging
// @visibility external
//<

//> @groupDef remoteDebugging
//
// In Pro and better builds (and also the Eval build), the SmartClient
// +link{groupDev:debugging,Developer Console} supports debugging remote pages,
// including those running on mobile devices.  With remote debugging, you can use all of the
// powerful debugging features of the +link{groupDev:debugging,Developer Console} - the
// component hierarchy (Watch tab), client/server requests (RPC tab), logs and log categories -
// using the large screen and physical keyboard of a desktop machine.
//
// <P>
// <h4>Using Remote Debugging</h4>
// <P>
// To enable remote debugging on a page, just add <code>isc_remoteDebug=true</code> to the page
// URL.  For example:
// <p>
// <smartclient>
// +externalLink{http://localhost:8080/isomorphic/system/reference/SmartClient_Explorer.html?isc_remoteDebug=true}
// </smartclient>
// <smartgwt>
// +externalLink{http://localhost:8080/?isc_remoteDebug=true}
// </smartgwt>
// </p>
// Note in the URL above, set localhost to the actual hostname or IP address of the machine
// running the SDK.
// <p>
// Then direct your <i>desktop</i> browser to the Developer Console:
// <p>
// <smartclient>
// +externalLink{http://localhost:8080/isomorphic/system/helpers/Log.html}
// </smartclient>
// <smartgwt>
// +externalLink{http://localhost:8080/showcase/sc/system/helpers/Log.html}
// </smartgwtt>
// </p>
// At top right of the page, you will see a "Remote" dropdown that lists the devices and URLs
// that have registered for
// remote debugging (by passing the <code>isc_remoteDebug</code> parameter).  As you roll over
// the available remote targets in this dropdown, the target page will glow blue to make it
// easy to tell which page you will be selecting for debugging - this is particularly handy
// when you have a lot of devices.  Pick the page to debug and just starting using the
// +link{groupDev:debugging,Developer Console} as normal.
// <p>
// If you reload the page on your mobile device, the remote Developer Console automatically
// re-establishes the connection.  And any settings - such as Logging Preferences or Watch tab
// settings - automatically persist as they normally would.
// <p>
// <h4>Licensing</h4>
// <p>
// Anyone with a Pro or better license can use the Remote Debugging feature.
//
// Under the covers, the Remote Debugging feature actually uses the Real-time Messaging module,
// which is not a Pro feature.  However we've rearranged things so that Pro users can use
// Real-time Messaging <i>just for Remote Debugging</i>.
//
// This does mean that, if you are upgrading your environment to the current release and you
// don't already have Real-time Messaging, you will need to follow the installation steps
// normally required for Real-time Messaging before the Remote Debugging feature will work.
// See the +link{groupDef:messaging} documentation for details.
//
// @title Remote Debugging
// @treeLocation Concepts
// @see debugging
// @visibility external
//<


//> @groupDef debugModules
// <smartclient>
// SmartClient comes with a debug / readable version of the SmartClient JS files that may
// be useful during development.
// </smartclient><smartgwt>
// Smart&nbsp;GWT LGPL, Pro, Power, and Enterprise come with debug / readable versions of the
// SmartClient JS files that may be useful during development.
// </smartgwt>
// <P><b>
// Note: These are useful only if you are interested in step-through debugging of
// framework JavaScript code using a JavaScript debugger, and we strongly discourage
// this as a primary approach to debugging: the SmartClient framework code provides many,
// many advanced features and is extremely sophisticated as a result.  Learning the
// internals of large parts of SmartClient is unnecessary and ineffective as a debugging
// approach, and the other approaches discussed in +link{group:debugging}
// should be your primary approaches to troubleshooting.</b>
// <smartclient>
// <p>To use the debug modules, simply change each &lt;script&gt; tag's SRC to the URI of the
// debug version of the module. For example:
// <pre>&lt;script src="/isomorphic/system/modules/ISC_Core.js"&gt;&lt;/script&gt;</pre>
// should be changed to:
// <pre>&lt;script src="/isomorphic/system/modules<b>-debug</b>/ISC_Core.js"&gt;&lt;/script&gt;</pre>
//
// <p>Alternatively, the &lt;isomorphic:loadISC&gt; and &lt;isomorphic:loadModules&gt; tags
// support a <code>useDebugModules</code> attribute:
// <pre>&lt;isomorphic:loadISC skin="Enterprise" useDebugModules="true"/&gt;</pre>
// <p>
// Note that the debug modules are intended to help in debugging your own application,
// and not the SmartClient Feature Explorer.  The Feature Explorer is not present
// in modules-debug, and the non-debug version requires the other non-debug files.</smartclient>
// <smartgwt>
// <p>To enable the use of debug modules, you will need to change the &lt;inherits&gt; lines
// in the application's GWT module file to reference the debug versions of the Smart&nbsp;GWT modules:
// <table border="1" cellpadding="5" cellspacing="0">
// <tbody>
// <tr><th>Edition</th><th>Original &lt;inherits&gt;</th><th>New &lt;inherits&gt;</th></tr>
// <tr>
// <th>LGPL</th>
// <td><code>&lt;inherits name="com.smartgwt.SmartGwt"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwt<b>.debug.</b>SmartGwt<b>Debug</b>"/&gt;</code></td>
// </tr>
// <tr>
// <th>Pro</th>
// <td><code>&lt;inherits name="com.smartgwtpro.SmartGwtPro"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwtpro<b>.debug.</b>SmartGwtPro<b>Debug</b>"/&gt;</code></td>
// </tr>
// <tr>
// <th>Power</th>
// <td><code>&lt;inherits name="com.smartgwtpower.SmartGwtPower"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwtpower<b>.debug.</b>SmartGwtPower<b>Debug</b>"/&gt;</code></td>
// </tr>
// <tr>
// <th>Enterprise</th>
// <td><code>&lt;inherits name="com.smartgwtee.SmartGwtEE"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwtee<b>.debug.</b>SmartGwtEE<b>Debug</b>"/&gt;</code></td>
// </tr>
// </tbody>
// </table>
// <p>
// The convention is that the names of debug GWT modules end with "Debug".
//
// <p>If using the NoScript modules, you will instead need to change the &lt;inherits&gt; lines
// as follows:<table border="1" cellpadding="5" cellspacing="0">
// <tbody>
// <tr><th>Edition</th><th>Original &lt;inherits&gt;</th><th>New &lt;inherits&gt;</th></tr>
// <tr>
// <th>LGPL</th>
// <td><code>&lt;inherits name="com.smartgwt.SmartGwtNoScript"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwt<b>.debug.</b>SmartGwtNoScript<b>Debug</b>"/&gt;</code></td>
// </tr>
// <tr>
// <th>Pro</th>
// <td><code>&lt;inherits name="com.smartgwtpro.SmartGwtProNoScript"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwtpro<b>.debug.</b>SmartGwtProNoScript<b>Debug</b>"/&gt;</code></td>
// </tr>
// <tr>
// <th>Power</th>
// <td><code>&lt;inherits name="com.smartgwtpower.SmartGwtPowerNoScript"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwtpower<b>.debug.</b>SmartGwtPowerNoScript<b>Debug</b>"/&gt;</code></td>
// </tr>
// <tr>
// <th>Enterprise</th>
// <td><code>&lt;inherits name="com.smartgwtee.SmartGwtEENoScript"/&gt;</code></td>
// <td><code>&lt;inherits name="com.smartgwtee<b>.debug.</b>SmartGwtEENoScript<b>Debug</b>"/&gt;</code></td>
// </tr>
// </tbody>
// </table>
// <p>
// and change the &lt;script&gt; tags in the application's HTML file to the debug modules
// instead of the normal SmartClient modules. For example:<pre><code>
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_Core.js"&gt;          &lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_Foundation.js"&gt;    &lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_Containers.js"&gt;    &lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_Grids.js"&gt;         &lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_Forms.js"&gt;         &lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_RichTextEditor.js"&gt;&lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_Calendar.js"&gt;      &lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_DataBinding.js"&gt;   &lt;/script&gt;
//    &lt;script src="myapp/sc/modules<b>-debug</b>/ISC_Drawing.js"&gt;       &lt;/script&gt;
// </code></pre>
// Alternatively, the &lt;isomorphic:loadISC&gt; and &lt;isomorphic:loadModules&gt; tags
// support a <code>useDebugModules</code> attribute:
// <pre>&lt;isomorphic:loadISC skin="Enterprise" useDebugModules="true"/&gt;</pre>
// </smartgwt>
//
// @title Using the Debug Modules
// @see debugging
// @treeLocation Concepts
// @visibility external
//<

// <smartgwt>
//> @groupDef superDevModeTroubleshooting
// This topic provides details on configuring and running Super Dev Mode, and troubleshooting
// any problems.  For an overview see +link{debugging, Debugging: Dev Mode and Super Dev Mode}.
// We suggest and describe setting up Super Dev Mode inside 
// +externalLink{https://www.eclipse.org/, Eclipse}, but the GWT Code Server can be run from
// the command line and the SGWT Web Application manually deployed to an existing server if
// desired.  It's assumed that you already have an Eclipse Project containing your Java code and
// a valid classpath picking up the SGWT JARs (perhaps the same project you use for Dev Mode)
// and that +externalLink{http://www.gwtproject.org/download.html, the GWT Eclipse Plugin}
// has been installed.
// <p>
// <h4>Creating a Run Configuration for the Code Server</h4>
// <p>
// You must first create a new Run Configuration for the GWT Code Server.  To do this:<ul>
// <li> Right Click / Run As... / Run Configurations
// <li> Select "Java Application", and hit the "New" button
// <li> Set the title (very top) to something you'll remember
// <li> Set the "Main class" to <code>com.google.gwt.dev.codeserver.CodeServer</code>
// <li> In the "Classpath" tab, add <code>gwt-codeserver.jar</code> using the
//      "Add External Jar" button
// <li> In the "Arguments" Tab, add entries for (at a minimum) the source path and 
//      the module (package, plus name of your .gwt.xml file) - for example:
//      <code>-src src/ com.smartgwt.sample.BuiltInDS</code></ul>
// <p>
// For the required JAR above, you can either 
// +externalLink{http://www.gwtproject.org/download.html, download} a version of the GWT SDK
// and extract the needed JAR, or locate it in your Eclipse installation (from the GWT Plugin).
// Additional arguments beyond those mentioned in the last step above are supported in the
// "Arguments" Tab, such as specifying a port, bind address, etc.  Your Run Configuration 
// should now be complete.
// <p>
// <h4>Configuring your GWT Project</h4>
// <p>
// A few additions to your GWT Project must be made if you're using a GWT SDK version older
// than 2.7.  (They are included as comments in the "Built in DS" sample GWT Project, 
// BuiltInDS.gwt.xml.)  If you're running GWT SDK 2.6 or older, you must add:
// <p>
// <code>&lt;add-linker name="xsiframe" /&gt;</code>
// <p>
// and if you're running GWT SDK 2.5 or older, you must also add:
// <p>
// <code>&lt;set-configuration-property name="devModeRedirectEnabled" value="true" /&gt;</code>
// <p>
// Super Dev mode does not support including modules which load javascript files, so if you're
// inheriting the standard module <code>com.smartgwtee.SmartGwtEE</code>, you'll need to switch
// that to <code>com.smartgwtee.SmartGwtEENoScript</code>, and add the following lines to your
// bootstrap HTML file (under the "war" directory):
// <pre><code>
//    &lt;script src="[app]/sc/modules/ISC_Core.js"&gt;          &lt;/script&gt;
//    &lt;script src="[app]/sc/modules/ISC_Foundation.js"&gt;    &lt;/script&gt;
//    &lt;script src="[app]/sc/modules/ISC_Containers.js"&gt;    &lt;/script&gt;
//    &lt;script src="[app]/sc/modules/ISC_Grids.js"&gt;         &lt;/script&gt;
//    &lt;script src="[app]/sc/modules/ISC_Forms.js"&gt;         &lt;/script&gt;
//    &lt;script src="[app]/sc/modules/ISC_RichTextEditor.js"&gt;&lt;/script&gt;
//    &lt;script src="[app]/sc/modules/ISC_Calendar.js"&gt;      &lt;/script&gt;
//    &lt;script src="[app]/sc/modules/ISC_DataBinding.js"&gt;   &lt;/script&gt;
//
//    &lt;script src="[app]/sc/skins/load_skin.js"&gt;&lt;/script&gt;
// </code></pre>
// Replace "<code>[app]</code>" with the directory containing the "sc" lib - determined by
// the "rename-to" attribute in your .gwt.xml file -- for example "builtinds" or "dsdmi".
// <p>
// <h4>Running the Code Server</h4>
// <p>
// At this stage you should be able to start the code server:<ul>
// <li> Right Click / Run As... / Run Configurations 
// <li> Select the new configuration you added</ul>
// <p>
// You should see a bunch of logging in the console tab of Eclipse, followed by a URL.  Visit
// that URL in your browser, and drag the "Dev Mode On" and "Dev Mode Off" buttons up to your
// browser bookmarks toolbar.  These bookmarks allow you to easily switch to Super Dev Mode
// (recompiling your Web Application) or switch back to Production Mode.
// <p>
// <h4>Deploying your Web Application</h4>
// <p>
// You should now launch the "Web Application" Run Configuration that it was suggested you use -
// the simplest way is to Right Click on the Project / Run As / Web Application.  Visit the URL
// generated by Eclipse, without the <code>gwt.codesvr</code> parameter.  You may see a
// warning about needing a recompile - you can either ignore this or run a full compile once to
// get rid of it.  (If you've deployed your Web Application manually outside of Eclipse, this
// section can be skipped.)
// <p>
// <h4>Entering Super Dev Mode</h4>
// <p>
// Visit the Production Mode URL of the running Web Application.  The bookmarks created earlier
// can now be used to enter Super Dev Mode:<ul>
// <li>hit the "Dev Mode On" bookmark link
// <li>on the pop-up, select the button to "Compile"</ul>
// <p>
// If you make code changes, you can update Super Dev Mode using the "Dev Mode On" bookmark.
// <p>
// As discussed in +link{debugging, Debugging: Dev Mode and Super Dev Mode}, not all browsers
// and debuggers at this time support the Source Maps feature that's required to set
// breakpoints for Super Dev Mode.  To enable them in Chrome, make sure the "Enable JavaScript
// Source Maps" checkbox is ticked in the Developer Tools preferences.  When the page is loaded
// and you've hit the "Dev Mode On" bookmark, you can browse the Java source in the debugger
// (under the "sources tab"), and set breakpoints in Java code.
// <p>
// <u><b>Troubleshooting</b></u>
// <table width="90%" class="normal" align="center" border="1" cellpadding="5">
// <tr bgcolor="#b0b0b0">
// 	<td width="30%"><b>Problem</b></td>
// 	<td width="30%"><b>Possible Causes</b></td>
// 	<td width="40%"><b>Solution</b></td>
// </tr><tr>
// <td>Missing GWT classes or JARs are reported when project is compiled.</td>
// <td>GWT Eclipse Plugin with GWT SDK is not installed or project was built with version
//  different from what's configured in Eclipse and needs to be rebuilt.</td>
// <td>Install plugin (ticking checkbox for GWT SDK) from 
// +externalLink{http://www.gwtproject.org/download.html, here} and/or rebuild project.</td>
// </tr><tr>
// <td>Errors are reported by GWT about the "linker not supporting script tags" when your
// project is oompiled.</td>
// <td>As mentioned above in "Configuring your GWT Project," Super Dev mode does not support
//  including modules which load javascript files.</td>
// <td>Follow the instructions in "Configuring your GWT Project" to switch to NoScript inherits
//  directives, and rebuild.</td>
// </tr><tr>
// <td>Nothing happens when you visit the "Dev Mode On" bookmark.</td>
// <td>The GWT Code Server is not running or the bookmark is not valid.</td>
// <td>Start the Code Server or create new bookmarks from the URL displayed in the Eclipse
// console when the Code Server launches.</td>
// </tr><tr>
// <td>No GWT projects are found/available for recompiling when the modal dialog opens
// from clicking "Dev Mode On" bookmark, and you're running GWT 2.6 or earlier.</td>
// <td>Needed lines of the GWT Project file (gwt.xml) are missing or commented.</td>
// <td>Read section "Configuring your GWT Project" above and uncomment the appropriate
// gwt.xml lines based on your GWT version, or copy them from BuiltInDS.gwt.xml</td>
// </tr><tr>
// <td>When you launch your Web Application from Eclipse as directed above, you see an error
// in the browser indicating the GWT Plugin is missing (and perhaps that it's not available).
// </td><td>
// You've forgotten to remove the <code>gwt.codesvr</code> argument from the URL as
// we instructed above and the browser is not able to find the GWT Dev Mode Plugin.</td>
// <td>Remove the <code>gwt.codesvr</code> argument from the URL, or don't use the Eclipse GWT
// "Web Application" Run Configuration template (intended originally for Dev Mode) to launch
// your Web Application.  You may deploy your Web Application manually outside of Eclipse.</td>
// </tr></table>
// <p><b>
// A useful discussion of some other problems and solutions related to GWT Super Dev Mode can be
// found +externalLink{http://stackoverflow.com/questions/18330001/super-dev-mode-in-gwt, here}.
// </b>
//
// @title Troubleshooting Super Dev Mode
// @treeLocation Concepts
// @see debugging
// @visibility external
//<
// </smartgwt>

//> @groupDef devConsoleRPCTab
// The "RPC" tab of the SmartClient Developer Console allows you to track 
// +link{class:RPCRequest}s and +link{class:DSRequest}s sent from your application.  Tracking
// is activated by checking the "Track RPCs" box at the top of the tab.
// <p>
// The main "RPC History" list shows the transactions that have been sent from your application
// since the session began (since you checked the "Track RPCs" box or refreshed your browser,
// whichever happened most recently).  Each entry in the list represents either a server
// roundtrip, a DSRequest to a clientOnly DataSource or a direct request to a webservice.  
// +link{RPCManager.startQueue,Request queues} are shown as separate entries, with the requests
// that made up the queue shown indented beneath it.
// <p>
// Each entry in the RPC History list shows useful diagnostic information, including:
// <ul>
// <li>Whether the request was sent via a server-side proxy</li>
// <li>The URL of the request, or an indication that the request was client-only</li>
// <li>The type of request - +link{class:DSRequest}, +link{class:RPCRequest}, 
//    +link{class:WSRequest} or +link{RPCManager.startQueue,Queue}</li>
// <li>The DataSource name, operation type and operation ID</li>
// <li>The success/failure status of the request, if it has completed</li>
// <li>Basic timing information</li>
// </ul>
// In addition, clicking an entry in the RPC History list populates the "Request" and "Response"
// sections with the details of the request.
// <p>
// <h2>Detailed timing/profiling information</h2>
// <P>
// SmartClient and SmartClient Server can gather detailed profiling information for a 
// request/response roundtrip, and display it in the Developer Console.  Note, the server-side
// information is only available for DSRequests, and only if you are using the SmartClient 
// Server module.  Extra levels of server-side detail are available if you are also using one 
// of SmartClient Server's built-in DataSource types (note, at the time of writing this only 
// applies to SQLDataSource).  To enable detailed timings:
// <ul>
// <li>Set debug log category "RpcTabTiming" to INFO level in "Logging Preferences" (see 
//     +link{group:debugging} for details)</li>
// <li>If you want to collect details of the server-side processing, either:
//     <ul>
//     <li>Set <code>DSRequest.returnTimingData: true</code> in your +link{server_properties,server.properties} 
//         file.  This will cause server timing information to be gathered and returned for 
//         every DSRequest from every client</li>
//     <li>Enable the built-in RPCs "areServerTimingsTracked" and "trackServerTimings" via the
//         <code>RPCManager.enabledBuiltinMethods</code> setting of your 
//          +link{server_properties,server.properties}
//         file (these builtin RPCs should already be enabled in a development environment). 
//         When these built-in RPCs are enabled, server timing data can be switched on and 
//         off on a per-client basis, via a checkbox in the Developer Console.</li>
//     </ul> 
// </li>
// </ul>
// With these settings in place, an extra "Timing" tab appears in the "Request" section:
// <p>
// <img src="skin/detailedTiming1.png" width="1059px" height="275px">
// <p>
// The timing data is tree-structured; a node with an opener next to it can be expanded to 
// drill into more detail:
// <p>
// <img src="skin/detailedTiming2.png" width="1059px" height="701px">
// <p>
// The following important points apply to the detailed timing information:
// <ul>
// <li>It is meaningless to compare the actual start and end timestamps reported by the client
//     with those reported by the server, because their clocks are unlikely to be exactly 
//     synchronized (unless the client and the server are the same physical machine).  The 
//     timestamps are the underlying raw data - it is much more meaningful to consider the 
//     elapsed times than the timestamps</li>
// <li>The basic timing details reported in the main RPC History list do not correspond to the
//     detailed timing data because the detailed timing data attempts to cover the entire 
//     period of the transaction, from the UI event to calling the user callback method.  By 
//     contrast, the basic timing data only covers the period from when the request left the 
//     client to when the response was received by the client.  The basic timing "Time Sent" 
//     and "Elapsed time" figures correspond to the "Server roundtrip" entry in the detailed 
//     timing data</li>
// <li>The "Network time (inferred)" measurements are informed guesswork.  We calculate the
//     difference between the "Server turnaround" time measured by the client and the "Server
//     processing" time measured by the server, and apply half of that difference to either side
//     of the "Server processing" figure as "Network time (inferred)".  Note that this guesswork
//     can easily mean that network timings overlap with server processing timings, even when the 
//     client and the server are the same machine</li>
// <li>The "UI event to DSRequest creation" timing measures the time from the most recent event 
//     to be registered by the EventHandler subsystem, to that of DSRequest creation (and 
//     hence, incidentally, is not recorded for RPCRequests).  This is often a meaningful thing
//     to measure, but not always.  If the DSRequest was created programatically during application 
//     startup or off the back of a timer, then the most recent UI event clearly had no influence 
//     and so measuring the time since it happened has no meaning.  However, most DSRequests 
//     <em>are</em> created, directly or indirectly, as a result of a UI event; so even though we 
//     can't tell which DSRequests belong to events and which don't, we still provide the figure 
//     in the timing data as something that will be "often useful"</li>
// </ul>
// 
// @title The Developer Console RPC Tab
// @treeLocation Concepts
// @visibility external
//<


isc.Log.addClassProperties({
	//> @type	LogPriority
    // Priority levels for log messages
    // @value  Log.FATAL   unrecoverable error
	FATAL : 1,
    // @value  Log.ERROR   error, may be recoverable
	ERROR : 2,
    // @value  Log.WARN    apparent problem, misused API, partial result
	WARN : 3,
    // @value  Log.INFO    significant events in normal operation
	INFO : 4,	
    // @value  Log.DEBUG   diagnostics for developers
	DEBUG : 5,
    // @see Class.logDebug()
	//			@visibility external
	//<
	
	//>	@classAttr	Log.PRIORITY_NAMES		(string[] : [...] : IRWA)
	//		User-visible names for log priorities
	//		Note: NONE should never show up...
	//<
	PRIORITY_NAMES :["NONE" ,"FATAL", "ERROR", "WARN", "INFO", "DEBUG"]

});


// Hide the Log class setup when we're not debugging
//	this lets us just include the logger, but makes it all a no-op.
//	Note that the creation of Log and the setting of the LogPriority must always be present.


isc.Log.addClassProperties({

    //>	@classAttr	isc.Log.defaultPriority		(LogPriority : isc.Log.WARN : IRWA)
	// Any logs below this priority will be suppressed, unless a more specific setting exists for
    // the category.
    // @see Log.setPriority() 
    // @visibility external
	//<
	defaultPriority:isc.Log.WARN,	

    //>	@classAttr	isc.Log.stackTracePriority (LogPriority : isc.Log.ERROR : IRWA)
	// At this priority and above, a stack trace will be included automatically along with the log
    // message itself.
    // @visibility external
	//<
	stackTracePriority:isc.Log.ERROR, 

    //>	@classAttr	isc.Log.showFireBugTrace (boolean : null : IRW)
    // Controls whether stack traces are written to the Firebug console.  
    // Any value except false permits stack traces to be written.
    //<
    //showFireBugTrace: false,

	// priorities setting per category
    _logPriorities: {
        "sgwtInternal": (
                         isc.Log.FATAL)
    },
    // specific priorities for classes / instances
    _objectLogPriorities: {},

	// number of messages to keep
	_messageCount:1000,
    

	// index of the slot for the next message in messageCache
	_messageIndex:0,				

	// array for keeping log messages
	_messageCache:[],

    _semiColon : ":",
    _dot : ".",
    _allCategories : "_allCategories",
    _default : "_default"
});

isc.Log.addClassMethods({

    // Log Priorities
	// --------------------------------------------------------------------------------------------

    //> @classMethod Log.applyLogPriorities()
    // Apply a batch a batch of priority settings, as a object mapping category names to priority
    // levels.
    //
    // @param settings (Object) priority settings for multiple categories
    // @visibility external
    //<
	applyLogPriorities : function (newDefaults) {
		// make a blank priority defaults object if necessary
		if (!this._logPriorities) {
            this._logPriorities = {};
        }            

		// if new defaults were specified, overlay them on the current set
		if (newDefaults) {
			isc.addProperties(this._logPriorities, newDefaults);
		}
	},
	
    //> @classMethod Log.getLogPriorities()
    // Get all priority settings as an object mapping category names to priority levels.
    //
    // @param [object] (Class or Instance object) Optional param to get priorities specific to
    //                                            some ISC class or instance.
    // @param [overridesOnly] (boolean) If this method is retrieving the priorities specific 
    //                                  to logging for some class or instance, this parameter
    //                                  can be used to view only the overrides to the default
    //                                  log priorities on this object.
    // @return (Object) priority settings
    // @visibility external
    //<
	getLogPriorities : function (object, overridesOnly) {
        var overrides;
        if (object != null) {
            var objectID = this._getObjectID(object);
            
            overrides = this._objectLogPriorities[objectID];
            if (overridesOnly) {
                return isc.addProperties({}, overrides);
            }
        }
        
        // copy to avoid unintentional changes
        var priorities = isc.addProperties({}, this._logPriorities);
        if (overrides) priorities = isc.addProperties(priorities, overrides);
        
        return priorities;
	},
    
    
    _getObjectID : function (object) {
        var ID;
        if (object == null) ID = isc.emptyString;
        else if (isc.isA.String(object)) return object; // already an ID
        else ID = (object.getID ? object.getID() : object.getClassName());
        return ID;
    },
	
    //> @classMethod Log.getPriority()
	// Return the priority setting for a particular category.
    // <P>
    // If there is no priority setting specific to this category, <code>null</code> will be
    // returned, NOT <code>Log.defaultPriority</code>.
    //
    // @param   category   (String)            category name
    // @param [object] (Class or Instance object)   Optional class or instance to check for 
    //                                              specific log priority overrides
    // @return  (LogPriority)     priority setting
    // @visibility external
    //<
	// return the priority for a particular category
	getPriority : function (category, object) {
        
        if (object != null) {
            var objectID = this._getObjectID(object),
                overrides = this._objectLogPriorities[objectID];
            if (overrides) {
                if (overrides._allCategories != null) return overrides._allCategories;
                if (overrides[category] != null) return overrides[category];
                if (overrides._default != null) return overrides._default;
            }
        }
        
        // Still going - look at global settings
        var priorities = this._logPriorities;
        return priorities[category] || priorities._default;
	},

    //> @classMethod Log.setPriority()
    // Set the priority of messages that will be visible for this log category.
    // <P>
    // After calling setPriority, any messages logged to the given category whose priority is
    // below the specified priority will not appear in the Log.
    //
    // @param category   (String)            category name
    // @param priority   (LogPriority)  priority level to set
    // @param [object]   (Class or Instance object) 
    //      Optional ISC class or instance - if passed the priority will be set for logging 
    //      occurring on the class or instance only.
    // @see Log.isEnabledFor() to check whether a category would allow a log at a given priority
    // @visibility external
    //<
	setPriority : function (category, priority, object) {
        if (object != null) {
            var objectID = this._getObjectID(object);
            if (this._objectLogPriorities[objectID] == null) 
                this._objectLogPriorities[objectID] = {};
            // If we're not passed a category, ensure we show all logs on the object in question
            // at the appropriate priority.
            if (!category) category = this._allCategories;
            this._objectLogPriorities[objectID][category] = priority;
        } else {
        
            this._logPriorities[category] = priority;
        }
	},
    
    //> @classMethod Log.setDefaultPriority()
    // Set the default priority of messages that will be visible.
    //
    // @param priority   (LogPriority)  priority level to set
    // @param [object]   (Class or Instance object) 
    //      Optional ISC class or instance - if passed the default priority will be set for logging 
    //      occurring on the class or instance only.
    // @visibility external
    //<
    setDefaultPriority : function (priority, object) {
        if (!object || object == isc.Log) isc.Log.defaultPriority = priority;
        else isc.Log.setPriority("_default", priority, object); 
    },
    
    //> @classMethod Log.getDefaultPriority()
    // Retrieves the default priority of messages that will be visible.
    //
    // @param [object]   (Class or Instance object) 
    //      Optional ISC class or instance - if passed the returns the default priority for 
    //     the class or instance only.
    // @return (LogPriority) default priority for which messages will be logged.
    // @visibility external
    //<
    getDefaultPriority : function (object) {
        var defaultPriority;
        if (object && object != isc.Log) defaultPriority = this.getPriority("_default", object);
        return defaultPriority || isc.Log.defaultPriority;
    },

    //> @classMethod Log.clearPriority()
	// Clear the priority setting for a particular category, so that the category's effective
    // priority returns to <code>Log.defaultPriority</code><br>
    // If the optional second parameter is passed, the specific priority setting for the 
    // category on that object will be cleared, so logs in that category on that object will
    // be logged at the global priority level for the category.
    //
    // @param category   (String)            category name
    // @param [object] (Class or Instance object) Optional instance or class object - if passed
    //                                        clear logging priority for the appropriate category
    //                                        on that object.
    // @visibility external
    //<
	clearPriority : function (category, object) {
        if (object) {
            var objectID = this._getObjectID(object);
            
            // If we were passed no category, clear all explicit log priorities on the object
            // in question.
            if (!category) 
                delete this._objectLogPriorities[objectID];
            else if (this._objectLogPriorities[objectID]) 
                delete this._objectLogPriorities[objectID][category];
            
        } else {
            delete this._logPriorities[category];
        }
	},

    //> @classMethod Log.isEnabledFor()
	// Would a message logged to the given category at the given priority appear in the Log?
    // <P>
    // NOTE: if there is no specific priority setting for a given category, the
    // <code>Log.defaultPriority</code> is used.
    //
    // @param category   (String)            category name
    // @param priority   (LogPriority)  priority level to check
    //
    // @visibility external
    //<
    // NOTE: hierarchical categories are not documented; not clear whether we want to expose this
    // feature
	isEnabledFor : function (category, priority, object) {
        if (!category) category = isc._emptyString;
		while (category != isc._emptyString) {
        
			// get the priority for the category
			var categoryPriority = this.getPriority(category, object);
			// if it was found and its priority is set
			if (categoryPriority != null) {
				// return if the message is at the appropriate priority
				return priority <= categoryPriority;
			}
			
			// if the category contains a period, chop it down and try again
			var periodIndex = category.lastIndexOf(this._dot);
			if (periodIndex > 0) {
				// chop off the last category
				category = category.substring(0, periodIndex);
			} else {
				// jump out of the loop
				break;
			}	
		}

		// category not found or was null -- return according to the default logging priority
		return priority <= isc.Log.defaultPriority;
	},

    // Formatting and Displaying Log messages
	// --------------------------------------------------------------------------------------------

	// log a message at an arbitrary priority (for wrappers)
	log : function (priority, message, category, msgPrefix, object, timestamp) {
		if (this.isEnabledFor(category, priority, object))
			this.addLogMessage(priority, message, category, msgPrefix, timestamp);
        else if (this.reportSuppressedLogs) {
            // Useful for detecting unnecessary logs, especially unnecessary logs during
            // critical path code
            this.logWarn("suppressed log, category: " + category + ": " + message
                // + this.getStackTrace()
            );
        }
	},

    // get a timestamp suitable for our short-lived log: millisecond precision, no need to show
    // date 
    
    _1zero : "0",
    _2zero : "00",
	getLogTimestamp : function (date) {
        var tsArray = this._tsArray;
        if (tsArray == null) {
            tsArray = this._tsArray = []; 
            tsArray[2] = this._semiColon;
            tsArray[5] = this._semiColon;
            tsArray[8] = this._dot;
        }

		if (date == null) date = new Date();
        var hours = date.getHours(),
            minutes = date.getMinutes(),
            seconds = date.getSeconds(),
            ms = date.getMilliseconds();
         
        tsArray[1] = hours;
        if (hours < 10) tsArray[0] = this._1zero;
        else tsArray[0] = null;

        tsArray[4] = minutes;
        if (minutes < 10) tsArray[3] = this._1zero;
        else tsArray[3] = null;

        tsArray[7] = seconds;
        if (seconds < 10) tsArray[6] = this._1zero;
        else tsArray[6] = null;

        tsArray[10] = ms;
        if (ms < 10) tsArray[9] = this._2zero;
        else if (ms < 100) tsArray[9] = this._1zero;
        else tsArray[9] = null;
        
        return tsArray.join(isc._emptyString);
	},


	// return the name shown to the user for a particular log priority
	getPriorityName : function (priority) {
		if (priority == null) return isc._emptyString;
		return this.PRIORITY_NAMES[priority];
	},

	// routine to format the log message and officially "log" it
	// override to set your own outputter
	_makeLogMessage : function (priority, message, category, msgPrefix, timestamp) {
        var msg = this._msgArray;
        if (msg == null) {
            msg = this._msgArray = [];
        }
        
		if (!category) category = this.category;
		
        msg[0] = this.getLogTimestamp(timestamp);
        msg[1] = this._semiColon;

        // Add the "thread" if available, eg, what the native source of the JS thread
        // is, such as mouse events, timers, etc
        if (this.ns.EH && this.ns.EH._thread != null) {
            msg[2] = this.ns.EH._thread;
            msg[3] = this._semiColon;
        }

        if (priority != null) {
            msg[4] = this.getPriorityName(priority);
            msg[5] = this._semiColon;
        }

        msg[6] = category;
        msg[7] = this._semiColon;
        // allow a prefix to the message to be passed in, so we can do the concat
        if (msgPrefix) {
            msg[8] = msgPrefix
            msg[9] = this._semiColon;
        }
        msg[10] = message;
		
		var result = msg.join(isc._emptyString);

        // clear out the array used to construct the message
        msg.length = 0;

        return result;
    },

	addLogMessage : function (priority, message, category, msgPrefix, timestamp) {
        

        var logMessage = this._makeLogMessage(priority, message, category, msgPrefix, timestamp);
		this.addToMasterLog(logMessage);

        if (this.warningLogged != null && priority != null && priority <= this.WARN) {
            this.warningLogged(logMessage);
        }

		// show alerts in addition for error and fatal level log messages
		if (priority != null && priority <= this.ERROR) {
			if (!isc.Browser.seleniumPresent) alert(message);
		}
	},

	// add a message to the master log
	// anyone who wants to know when messages are added should observe this method!
	addToMasterLog : function (message) {
//!DONTOBFUSCATE
// NOTE: we're not obfuscating so the "message" parameter will retain that name later
		// remember the message passed in
		this._messageCache[this._messageIndex] = message;

		// set up for the next message
		this._messageIndex++;
		
		// if we're beyond the appropriate number of messages to remember
		if (this._messageIndex > this._messageCount) {
			// roll over the messsageIndex to 0
			this._messageIndex = 0;
		}
		if (this.showInlineLogs) {
		    this.updateInlineLogResults();
		}
	},
	
	showInlineLogs:false,
	updateInlineLogResults : function () {
	    if (isc.Canvas == null || this._messageCache == null) return;
	    if (!this.inlineLogCanvas) {
	        this.inlineLogCanvas = isc.Canvas.create({
	                width:"50%", height:"100%", overflow:"auto",
	                backgroundColor:"white",
	                canDragReposition:true,
	                autoDraw:true
	        });
	    }
	    this.inlineLogCanvas.setContents(this._messageCache.join("<br>"));
	    this.inlineLogCanvas.bringToFront();
	},
	
	// return the array of messages stored in the master log
	getMessages : function () {
		var cache = this._messageCache,
			index = this._messageIndex,
			count = this._messageCount
		;
		return cache.slice(count-index,count).concat(cache.slice(0, index));
	},
	
    //> @classMethod Log.show()
    // Open the Developer Console.
    // <P>
    // The Developer Console should <b>always</b> be open while developing any ISC-enabled
    // application, because ISC logs many important errors and warnings to the Developer Console.
    // <P>
    // In Internet Explorer, the Developer Console is able to log a stack trace for every JS error,
    // including errors that occur in non-ISC code.
    // <P>
    // NOTE: if you have the Microsoft JavaScript Debugger installed, ISC will be unable to log
    // stack traces on JS errors until you go to Tools->Internet Options->Advanced Tab and check
    // "Disable script debugging".  The ability to see stack traces in the Developer Console is
    // generally much more useful for debugging ISC-based applications than the generic Javascript
    // Debugging facilities.
    // 
    // @group debug
    // @visibility external
    //<
    show : function (loading, logWindow, dontSaveState, windowName, inline) {
        if (!this.logViewer) this.logViewer = isc.LogViewer.create();
        this.logViewer.showLog(loading, logWindow, dontSaveState, windowName, inline);
    },

    //> @classMethod Log.clear()
	// Clear all currently displayed Log messages
    // @visibility external
    //<
	clear : function () {
         
		this._messageCache = [];
		this._messageIndex = 0;
        if (this.logViewer) this.logViewer.clear(); 
	},
    
    // evaluate an expression and log the results
    evaluate : function (expr, evalVars) {
        // execute the expression - and always report execution time
        var start = isc.timeStamp();

        var error,
            result
        ;
        // NOTE: "this" is the Log so that this.logWarn, this.echo et al will work
        if (isc.Log.supportsOnError) {
            // in IE, if there's an error, we report it via window.onerror
            result = isc.Class.evalWithVars(expr, evalVars, this);
        } else {
            // NOTE: try {} catch is not supported in Safari11, Nav4, or IE4
            try {
                result = isc.Class.evalWithVars(expr, evalVars, this);
            } catch (e) {
                error = e;
            }
        }
        var end = isc.timeStamp(),
            // show a timestamp for the log message itself if enabled
            resultString = isc.Log.getLogTimestamp() + ":";
        
        // don't show the entire expression
        var lines = expr.split(/[\r\n]+/);
        if (lines.length > 1) expr = lines[0] + "...";
        if (expr.length > 200) expr = expr.substring(0,200) + "...";
        if (error) {
            if (!isc.Log.supportsOnError) {
                isc.Log._reportJSError(error);
                return;
            }

            // In IE the error is an object - get the description property.
            // Unused since we let errors fall through in IE
            //if (isc.Browser.isIE) error = error.description;
            
            resultString += "Evaluator: '" + expr + "' returned a script error: \r\n" 
                         + "'" + error + "'";
        } else {
            resultString = "Evaluator: result of '" + expr + "' (" + (end-start) + 
                "ms):\r\n" + this.echo(result);
        }
        // Use addToLog instead of addToMasterLog() 
        // - we don't care about losing this on log window reload
        if (this.logViewer) this.logViewer.addToLog(resultString, true);
	},

    // update the form in the log viewer
    updateStats : function (stat) {
        if (isc.debugTarget) isc.debugTarget.updateStats(stat);
    },

    // allow storing log messages before Log class has loaded (advanced internal usage)
    _logPrelogs : function () {
        var preLogs = isc._preLog;
        if (!preLogs) return;
        for (var i = 0; i < preLogs.length; i++) {
            var log = preLogs[i];
            if (isc.isA.String(log)) this.logDebug(log);
            else this.logMessage(log.priority || isc.Log.INFO,
                                 log.message, log.category, log.timestamp);
        }
        isc._preLog = null;
    },

    // Tracing and timing
	// --------------------------------------------------------------------------------------------

	//>	@classMethod		Log.traceMethod()
    //
    //  Observe a method on an object, logging a stack trace whenever the method is called.
    //  <P>
    //  Call a second time with identical arguments to disable tracing.
    //
	//	@param	object		(object)	object to observe
	//	@param	methodName	(string)	name of the method to observe
    //
	//	@group	debug
	//	@visibility external
    //<
    traceMethod : function (obj, methodName, callOnly) {
        if (!obj || !methodName) return;
        
    	// Bail if the arguments aren't valid
    	var object = this.validObservation(obj, methodName);
    	if (!object) return;
        
        // Keep a list of what objects / methods we're logging traces for
        //      Note: format is {objName:[methodName1, methodName2]}
        
        if (!this._traceRegistry) this._traceRegistry = {};
        if (!this._traceRegistry[obj]) this._traceRegistry[obj] = []; // array of method names

        // observation can only be done by instances, so create an arbitrary instance to
        // observe with
        if (!this._observer) this._observer = isc.Class.create();
        var observer = this._observer;

    	// If this object is already being traced, stop observation
    	if (observer.isObserving(object, methodName) && 
            this._traceRegistry[obj].contains(methodName)) 
        {
    		observer.ignore(object, methodName);
    		this.logWarn("MethodTimer: Stopped logging stack traces for " + methodName + 
                         " method on " + obj);
            // remove it from the registry        
            this._traceRegistry[obj].remove(methodName);
            
    	} else {
            var objName = object.ID ? object.ID : (object.Class ? object.Class : object),
                expression = "isc.Log.logWarn('" + objName + "." + methodName + "() - trace:' +";
            if (callOnly) {
                expression += "'\\n' + isc.Log.getCallTrace(arguments))";
            } else { 
                expression += "isc.Log.getStackTrace())";
            }
            this.logWarn("expression is: " + expression);
    		observer.observe(object, methodName, expression);
    		this.logWarn("MethodTimer: Logging traces whenever " + methodName + 
                         " method on " + obj + " is called");
            // add it to the registry
            this._traceRegistry[obj].add(methodName);                 
    	}
        
    },
    
    traceCall : function (obj, methodName) {
        this.traceMethod(obj, methodName, true);
    },

	//>	@classMethod		Log.timeMethod()
    // 
    //  Observe a method on an object, logging execution time whenever the method is called.
    //  <P>
    //  Call a second time with identical arguments to disable tracing.
    //
	//	@param	object		(object)	object to observe
	//	@param	methodName	(string)	name of the method to observe
    //
	//	@group	debug
	//	@visibility external
    //<
    // storeTotals: execution times of methods and totals for that method will be store in a
    //              central structure Log.classMethodTimes, like:
    //            {
    //                className: { // also "All"
    //                   totalTime:0, calls:0, minTime:0, maxTime:0, avgTime:0
    //                }
    //            }
    // dontLog: means that individual executions will not be logged (typically used with
    //          storeTotals:true when timing methods where logging itself would be significant)
    //
    // Typical use cases:
    // 1. profile a mixture of operations (eg lots of components drawing) to get a breakdown of
    //    time spent in particular methods
    //
    //    isc.Log.timeMethod(someClass, someMethod, true, true); // several times
    //    isc.Log.resetTotals();
    //    ... run test code ...
    //    isc.logWarn(isc.echoFull(isc.Log.classMethodTimes));
    // 
    // 2. time a specific codepath to see the time breakdown (high resolution timer only)
    //
    //    isc.Log.timeMethod(someClass, someMethod, true, false); // several times
    //    isc.Log.deferTimerLogs = true;
    //    ... run test code ...
    //    isc.Log.logDeferred(); // alll deferred logs are dumped
    //
	_methodPrefix:"$T_",
    timeMethod : function (obj, methodName, storeTotals, dontLog, causeGC) {
        if (!obj || !methodName) return;

    	// Bail if the arguments aren't valid
    	var object = this.validObservation(obj, methodName);
    	if (!object) return;

        // Keep a list of what objects / methods we're timing
        //      Note: format is {objName:[methodName1, methodName2]}
        if (!this._timeRegistry) this._timeRegistry = {};
        if (!this._timeRegistry[obj]) this._timeRegistry[obj] = []; // array of method names

        // already timing the method       
        if (this._timeRegistry[obj].contains(methodName)) return;

        // Note - to time the method, we rename it, and replace it with a timer method (which will
        // return the same value
    	var saveMethodName = isc.Log._methodPrefix + methodName,
    		observedMethod = isc._obsPrefix + methodName, // Observation saves original method as _$method
    		oldMethodName = (object[observedMethod] ? observedMethod : methodName)
    	;
 
        // If we're not timing the method:
    	// If the method isn't being observed, we save the original method on the object as
    	// (prefix + method) and replace it with a method that times and calls (prefix + method)
    	//
    	// If the method IS being observed, we do the same thing, except instead of saving and
    	// replacing the current method, we save and replace (isc._obsPrefix + method), which is where the
    	// original method's saved for observation.
    	//
    	// This way, we time only the original method, not the original method + its observer queue.
    	//
    	// This works even if we subsequently observe the method, because the method saved by the
    	// observation mechanism (isc._obsPrefix + method) is left untouched if it already exists.
        
    	object[saveMethodName] = object[oldMethodName];
    	object[oldMethodName] = isc.Log.makeTimerFunction(
    		methodName, object, storeTotals, dontLog, causeGC
    	);
    	this.logWarn("MethodTimer: Timing " + methodName + " method on " + obj);
        this._timeRegistry[obj].add(methodName);

    },
    
    stopTimingMethod : function (obj, methodName) {
    	// Bail if the arguments aren't valid
    	var object = this.validObservation(obj, methodName);
    	if (!object) return;

        // If we're already timing the method, stop timing it.
        if (this._timeRegistry[obj].contains(methodName)) {
    	    var saveMethodName = isc.Log._methodPrefix + methodName,
                // Observation saves original method as _$method
    		    observedMethod = isc._obsPrefix + methodName, 
        		oldMethodName = (object[observedMethod] ? observedMethod : methodName)

            if (!object[saveMethodName]) {
                // This should never happen but we'll just clean up by deleting the registry entry
                this.logWarn("Not timing method '" + methodName + "' on object '"+ obj +"'.");
                this._timeRegistry[obj].remove(methodName);
                return;
            }
            
            // Stop timing the method:
            object[oldMethodName] = object[saveMethodName];
            delete object[saveMethodName];
    		this.logWarn("MethodTimer: " + methodName + " method on " + obj + 
                         " is no longer being timed");            
            this._timeRegistry[obj].remove(methodName);
            return;
        }            
    },
	
    // generate a function that calls the original message and logs timing data
    _currentlyTiming:{},
	makeTimerFunction : function (methodName, object, storeTotals, dontLog, causeGC) {

		var method = object[methodName],
            fullMethodName = isc.Func.getName(method, true);

        

        
        var timerFunc = function (a,b,c,d,e,f,g,h,i,j,k) {
            // you can use this to take the GC-based variability out of a method being timed
            if (causeGC) isc.Log._causeGC();
			var start = isc.timeStamp();
            
            
            var returnValue = method.call(this, a,b,c,d,e,f,g,h,i,j,k);
            var total = (isc.timeStamp()-start);
 
            
                
            if (!dontLog) isc.Log._logTimerResult(this, fullMethodName, total);
            return returnValue;
        }
		timerFunc._fullName = (object.ID || object.Class || "") + "_" + methodName + "Timing";
        timerFunc._isTimer = true;
        timerFunc._origMethodSlot = isc.Log._methodPrefix + methodName;
        return timerFunc;
	},

    // logTimerResult: log the result of timing a method
    _timerMessage : [
        "Timed ", 
        , // methodName
        ": ",
        , // time
        "ms"
    ],
    _logTimerResult : function (object, methodName, callTime) {
        if (this.deferTimerLogs) return this._deferTimerLog(object, methodName, callTime);
        var template = isc.Log._timerMessage;
            
        // if "logWarn" exists, use it so the object identifies itself, otherwise, 
        // toString() the object as part of the log message
        template[1] = (object.logWarn ? methodName : 
                                        methodName + " on " + this.echoLeaf(object));
        template[3] = callTime.toFixed(3);
    
        var message = template.join(isc.emptyString);
        if (object.logMessage) object.logWarn(message);
        else isc.Log.logWarn(message);
    },

    

    

    // check whether method "method" on "obj" can be observed.  "obj" can be a string expression
    // that evaluates to an object
    validObservation : function (obj, method) {
    	// Check that both fields are defined
    	if (isc.isAn.emptyString(obj) || isc.isAn.emptyString(method)) return false;
    
        var object = obj;
        if (isc.isA.String(obj)) {
            // assume an expression (including a simple global ID)
            object = isc.Class.evaluate(obj);
            if (!object) {
                this.logWarn("MethodTimer: " + obj + " is not an object.");
                return false;
            }
        }
    
        // If the method was specifed with parentheses, remove them:
        if (method.indexOf("(") != -1) {
            method = method.slice(0, method.indexOf("("));
        }
        
    	// If the object is a class, then we check whether there's a static method or an instance
    	// method with the given name on the class.
    	if (isc.isA.ClassObject(object)) {
    		var theProto = object.getPrototype();
            // look for an instance method first and return the instance prototype if an
            // instance method was found
    		if (isc.isA.Function(theProto[method])) return theProto;

    		if (!object[method]) {
    			this.logWarn("MethodTimer: " + method + 
                             " could not be found as a static or instance property on " + obj);
    			return false;
    		}
    	// not a class object, check that the method exists on it.
    	} else if (!object[method]) {
    		this.logWarn("MethodTimer: " + method + " is undefined or null on " + obj);
    		return false;
    	}
    	
    	// Check that the method is in fact a function, and not some other type of object
    	if (!isc.Func.convertToMethod(object, method)) {
    		this.logWarn("MethodTimer: " + method + " is not a method on " + obj);
    		return false;
    	}
    	
    	// Passed all the checks, return the object
    	return object;
    },

    // Hiliting a Canvas -- XXX: move this to DebugTarget?
	// --------------------------------------------------------------------------------------------
    hiliteCanvas : function (name) {
        var canvas = name;
        if (isc.isA.String(name)) canvas = window[name];

        if (!isc.isA.Canvas(canvas)) {
            //>DEBUG
            this.logWarn("Unable to find specified canvas '" + name + "'."); //<DEBUG
            return;
        } 

        this.showHiliteCanvas(canvas.getPageRect());
    },

    hiliteElement : function (name) {
        var element = name || this.elementToHilite;
        if (isc.isA.String(name)) element = isc.Element.get(name);
        if (element == null) {
            //>DEBUG
            this.logWarn("Unable to find specified element '" + name + "'."); //<DEBUG
            return;
        } 

        this.showHiliteCanvas(isc.Element.getElementRect(element));
        this.elementToHilite = null;
    },

    showHiliteCanvas : function (rect) {

        // flash an outline around the canvas
        var hiliteCanvas = this._hiliteCanvas;
        if (!hiliteCanvas) {
            hiliteCanvas = this._hiliteCanvas = isc.Canvas.create({
                ID:"logHiliteCanvas",
                autoDraw:false,
                overflow:"hidden",
                hide : function () {
                    this.Super("hide", arguments);
                    this.resizeTo(1,1);
                    this.setTop(-20);
                },
                border1:"2px dotted red",
                border2:"2px dotted white"
            })
        }

        hiliteCanvas.setPageRect(rect);

        isc.Page.setEvent("click", hiliteCanvas.getID() + ".hide()");

        hiliteCanvas.setBorder(hiliteCanvas.border1);
        hiliteCanvas.bringToFront();
        hiliteCanvas.show();
        
        // Flash the border a few times
        this._flashHiliteCanvas()
    },
    
    hideHiliteCanvas : function () {
        if (this._hiliteCanvas) this._hiliteCanvas.hide();
    },

    flashHiliteCount: 7,
    flashHilitePeriod: 500,
    
    _flashHiliteCanvas : function () {
        // a function to set the hilite canvas to flash on a timer a few times
        var borders = [this._hiliteCanvas.border1,this._hiliteCanvas.border2];
        
        for (var i=0; i<this.flashHiliteCount; i++) {
            isc.Timer.setTimeout({
                    target:this._hiliteCanvas, methodName:"setBorder",
                    args:[borders[i%2]]
                }, (this.flashHilitePeriod*i)
            )
        }
    }
    
});


//	LogViewer -- simple log viewer -- use to display the log visually.
// ---------------------------------------------------------------------------------------
//	Automatically updates whenever the log is added to.

isc.ClassFactory.defineClass("LogViewer");
isc.LogViewer.addClassMethods({
    // the GlobalLogCookie stores Log window sizing info that's required to be at path / to
    // work. 
	getGlobalLogCookie : function () {
		var globalLogCookie = isc.Cookie.get("GLog");
		if (!globalLogCookie) return null;

        try {
            var fn = isc._makeFunction("return " + globalLogCookie);
	    	return fn();
        } catch (e) {
            this.logWarn("bad log cookie: " + globalLogCookie + this.getStackTrace());
        }
	},
    // Function to easily get a cookie property value
    getGlobalLogCookieValue : function (property) {
        // dontSaveState property may have been set up on launch
        if (window.dontSaveState) return "";
    
        var globalLogCookie = this.getGlobalLogCookie();
        if (globalLogCookie != null && globalLogCookie[property] != null) {
            return globalLogCookie[property];
        }
        return "";
    },
    setGlobalLogCookieValue : function (property, value) {
        var globalLogCookie = this.getGlobalLogCookie();
        if (!globalLogCookie) globalLogCookie = {};
        globalLogCookie[property] = value;

        this.setGlobalLogCookie(globalLogCookie);
    },
    setGlobalLogCookie : function (globalLogCookie) {
        // the global log cookie is associated with "/" so it is sent to all pages
        var serializedGlobalLogCookie = isc.Comm.serialize(globalLogCookie);
        isc.Cookie.set('GLog', serializedGlobalLogCookie, "/", null, this.getCookieExpires());
    },



    getCookieExpires : function () {
        // Code to set up the log cookie
        var expires = new Date();
        expires.setFullYear(expires.getFullYear() + 20);
        return expires.toGMTString();    
    },

    // The LogCookie is stored at /isomorphic/system/helpers so as not to pollute the / HTTP
    // header space.  This cookie contains everything except what the GlobalLogCookie has
	getLogCookie : function () {
		var logCookie = isc.Cookie.get("Log");
		if (!logCookie) return null;

        try {
            var fn = isc._makeFunction("return " + logCookie);
	    	return fn();
        } catch (e) {
            this.logWarn("bad log cookie: " + logCookie + this.getStackTrace());
        }
	},
    // Function to easily get a cookie property value
    getLogCookieValue : function (property) {
        // dontSaveState property may have been set up on launch
        if (window.dontSaveState) return "";
    
        var logCookie = this.getLogCookie();
        if (logCookie != null && logCookie[property] != null) return logCookie[property];
        return "";
    },
    setLogCookieValue : function (property, value) {
        var logCookie = this.getLogCookie();
        if (!logCookie) logCookie = {};
        logCookie[property] = value;

        this.setLogCookie(logCookie);
    },
    setLogCookie : function (logCookie) {
        // the global log cookie is associated with "/" so it is sent to all pages
        var serializedLogCookie = isc.Comm.serialize(logCookie);
        isc.Cookie.set('Log', serializedLogCookie, null, null, this.getCookieExpires());
    }    
});

isc.LogViewer.addMethods({

    // the debugOn flag is toggled by the log window when remote debugging is enabled to
    // prevent the opener from updating the log window with local data when it is watching
    // a remote
    _debugOn: true,

    // whether the log window is loaded and ready to be accessed
    logWindowLoaded : function () {
        // We get bizarre errors in IE (typically: "trying to execute a freed script") if we
        // try to access elements of the logWindow page from the main frame if the log window
        // is being loaded, and replacing an existing log window.
        // This is probably due to window.open() returning a handle that is in an invalid state
        // until the new log window finished loading.
        // Therefore we wait for the log window to actually call back to the main frame and set
        // a flag telling us it has loaded.
        // 
        return (this._logWindowLoaded && this._logWindow != null && !this._logWindow.closed );
    },

    // showInline - if true we show the full log console in an isc.Window rather than a separate
    // window. useful for tablets where multi-window is a pain but we actually have enough space
    // to basically work with a log window.
    
    showConsoleInline:isc.Browser.isTouch,
    
	showLog : function (loading, logWindow, dontSaveState, windowName, showInline) {
        if (!isc.debugTarget || isc.RemoteDebug.isEnabled) {
            var _this = this;
            isc.RemoteDebug.enableLocal(function () {
                _this.showLog(loading, logWindow, dontSaveState, windowName, showInline);
            });
            return;
        }

	    if (showInline == null) showInline = this.showConsoleInline;
        // allow a log window to be passed in.  This allows the log window to reconnect to the
        // opener after the opener has been navigated to a new ISC page.
        if (logWindow) this._logWindow = logWindow;

        //alert("showLog called: loading: " + loading + ", logWindow: " + this._logWindow + 
        //      ", form: " + (this._logWindow ? this._logWindow.resultsForm : null));
		//	if the _logWindow property is set up, it's a pointer to a log window we previously
        //  opened.  If we can get into its form, just replace the form contents which is much
        //  faster.
		if (this.logWindowLoaded()) {
            if (!this._logWindowInline) {
                this._logWindow.focus();
            }
            return;
        }
	
        // Assume that this is the only logViewer instance running - make sure it's available
        // as Log.logViewer
        if (!isc.Log.logViewer) isc.Log.logViewer = this;

        // if we have a log window, and it's not closed, we're done
        // (Note - if it is in the process of loading, we will rightly leave it alone)
        if (this._logWindow && !this._logWindow.closed) {
            
            return;
        }

        var rect = {},
            globalLogCookie = (dontSaveState ? null : isc.LogViewer.getGlobalLogCookie());

        if (globalLogCookie != null) {
            rect = globalLogCookie;
	        // Disabled due to multiple-monitors: the log window position that's saved doesn't
            // work properly unless the log window is in the primary monitor. Also, negative
            // coordinates will mean that the window will be displayed at (0, 0) instead.
            /*
            // make sure the log window doesn't end up off the screen
            rect.left = rect.left > screen.availWidth ? 0 : rect.left;
            rect.top = rect.top > screen.availHeight ? 0 : rect.top;
            */
        } else {
			rect.left = 100;
			rect.top = 100;
            rect.width = 640;
            rect.height = 480;
        }
        
        if (showInline) {
            if (this.inlineWindow == null) {
                this.inlineWindow = isc.Window.create({
                    title:"Inline Developer Console",
                    src:isc.Page.getIsomorphicClientDir() + "helpers/Log.html",
                    animateMinimize:false,
                    // Size big enough to interact with and small enough to be able to grab
                    // the resize edges easily.
                    width:"50%",
                    height:Math.round(isc.Page.getHeight() * 0.8),//"80%",
                    headerControls:[
                        "headerIcon",
                        "headerLabel",
                        isc.Button.create({
                            width:16,
                            height:14,
                            title:"TL",
                            layoutAlign:"center",
                            click:function() {
                                isc.Log.logViewer.inlineWindow.moveTo(0,0);
                            }
                        }),
                        isc.Button.create({
                            width:16,
                            height:14,
                            title:"BL",
                            layoutAlign:"center",
                            click:function() {
                                isc.Log.logViewer.inlineWindow.moveTo(0,
                                    isc.Page.getHeight()-isc.Log.logViewer.inlineWindow.getHeight());
                            }
                        }),
                        isc.Button.create({
                            width:16,
                            height:14,
                            title:"TR",
                            layoutAlign:"center",
                            click:function() {
                                isc.Log.logViewer.inlineWindow.moveTo(
                                    isc.Page.getWidth()-isc.Log.logViewer.inlineWindow.getWidth(),
                                    0);
                            }
                        }),
                        isc.Button.create({
                            width:16,
                            height:14,
                            title:"BR",
                            layoutAlign:"center",
                            click:function() {
                                isc.Log.logViewer.inlineWindow.moveTo(
                                    isc.Page.getWidth()-isc.Log.logViewer.inlineWindow.getWidth(),
                                    isc.Page.getHeight()-isc.Log.logViewer.inlineWindow.getHeight());
                            }
                        }),
                        "minimizeButton",
                        "maximizeButton",
                        "closeButton"
                    ],
                    showMaximizeButton:true,
                    showMinimizeButton:true,
                    canDragReposition:true,
                    canDragResize:true
                });
            }
            
            if (!this.inlineWindow.isDrawn()) {
                this.inlineWindow.draw();
            }
            this._logWindowInline = true;
        
        } else {
                    
            var windowSettings = "RESIZABLE,WIDTH=" + rect.width + ",HEIGHT=" + rect.height;
        
            if (globalLogCookie) {
                if (isc.Browser.isIE) {
                    windowSettings += ",left=" + rect.left + ",top=" + rect.top;
                } else {
                    windowSettings += ",screenX=" + rect.left + ",screenY=" + rect.top;
                }
                if (globalLogCookie.evals) this._currentEval = globalLogCookie.evals.length - 1;
                        
                
                        
            }
    
            
            //var subWindow = (window.opener && window.opener.isc);
            windowName = windowName || "_simpleLog";
    
            this._logWindow =
                window.open(isc.Page.getIsomorphicClientDir() + "helpers/Log.html", 
                            windowName
                            // avoid log window name collisions between Devenv and released
                            // versions of ISC.  NOTE: we'd use the version number, but 
                            // IE only is unhappy with a window name of eg "log5.5".
                            + (isc.version.contains("version") ? "Dev" : "")
                             , windowSettings);
        }

        this._initLogWindow(dontSaveState);
    },

    _logWindowInitAttempts:0,
    _logWindowPollInterval: 25,
    _initLogWindow : function (dontSaveState) {
        if (this._logWindow == null && this.inlineWindow != null) {
            var iFrame = this.inlineWindow.body._getURLHandle();
            if (iFrame) {
                this._logWindow = this.inlineWindow.body._getURLHandle().contentWindow;
            }
            // bail if we couldn't get the handle.
            
            if (this._logWindow == null) {
                return;
            }
        }
        
        if (this._logWindow == null) return;
        if (isc.Browser.isIE) {
            // if we've set document.domain, then attempting to immediately set a property on
            // the new window, before it can adjust its document.domain automatically, results
            // in an 'Access denied' error, so poll.
            try {
                this._logWindow._accessTest = true;
            } catch (e) {
                this.delayCall("_initLogWindow", [dontSaveState], this._logWindowPollInterval);
                return;
            }
        }

        // In IE, set up a pointer to this window in the newly opened log window
        // This is necessary as IE will not replace the 'window.opener' property to point
        // to this window, if the above call replaced the contents of an already open
        // log window.
        if (isc.Browser.isIE || this._logWindowInline) {
            this._logWindow.launchWindow = window;
            if (this._logWindowInline) {
                this._logWindow.showingInline = true;
            }
        }
        
        // If we don't want the log window to attempt to save / retrieve state from cookies
        // set a flag on it
        if (dontSaveState) this._logWindow.dontSaveState = true;
        
        
        // focus in the log window we just opened, to bring it in front of whatever other windows
        // might be occluding it (WinAmp et al).
        // Do this on an idle.  Otherwise some browsers will focus in the log window, then
        // as code continues to execute in the main window, focus back in the main window.
        // (Mac Moz is a specific example of this).
        //
        // Put the code to focus inside a conditional in case the window is dismissed before
        // page idle fires.
                
        var focusFunction = function () {
            if (isc.Log.logViewer) {
                var logWindow = isc.Log.logViewer._logWindow;
                if (logWindow && !logWindow.closed) logWindow.focus();
            }
        }

        // Note - if we're showing the log window on page load, avoid this
        isc.Page.setEvent("idle", focusFunction, isc.Page.FIRE_ONCE);

        // if the log window is already open, then reconnect.  Otherwise the log window will
        // fire initializePage() on its own onload.
        if (this._logWindow.initializePage) this._logWindow.initializePage();
	},
	
    // unlike addToMasterLog(), addToLog() simply updates the log window's results form
    // *without* putting the message into the message index.  This means the log would be lost
    // by log window reload, unlike normal logs.  Used by eval (above)
    // Standard logWarn() et al. use addToMasterLog() - observed by Log.html to keep the
    // results form up to date.
	addToLog : function (message, scrollToEnd) {
        if (isc.debugMaster && !this._tmpDisableLog) {
            // suppress logs caused by message send - e.g. something like a log serialization
            // causing an error which is then logged
            this._tmpDisableLog = true;
            isc.debugMaster.call("addToLog", [message, scrollToEnd]);
            delete this._tmpDisableLog;
        }
	},
    
    evaluate : function (expr, evalVars) {
        return isc.Log.evaluate(expr, evalVars);
	},

    clear : function () {
        if (isc.debugMaster) isc.debugMaster.call("clearLog");
    }
    
    
    
});

// Set up the preferences, log priorities etc. saved in a previous session
isc._globalLogCookie = isc.LogViewer.getGlobalLogCookie();
if (isc._globalLogCookie != null) {
    isc.Log.applyLogPriorities(isc._globalLogCookie.priorityDefaults)
    
    if (isc._globalLogCookie.defaultPriority != null) 
        isc.Log.defaultPriority = isc._globalLogCookie.defaultPriority;
} else {
    // For the "Log" category, default to "info"
    isc.Log.setPriority("Log", isc.Log.INFO);
}

isc.showConsole = function (loading, logWindow, dontSaveState, windowName) { 
    isc.showLog(loading, logWindow, dontSaveState, windowName); 
}
// this basically only exists as a convenience for those with old javascript:showLog() bookmarks
isc.addGlobal("showLog", function (loading, logWindow, dontSaveState, windowName) { 
    isc.Log.show(loading, logWindow, dontSaveState, windowName) 
})

// Useful for touch browsers so you can see log window and page content in the same browser view.
isc.addGlobal("showConsoleInline", function () {
    isc.Log.show(null, null, null, null, true);
});

// indicate that the log has started
isc.Log.logInfo("initialized");

// allow storing log messages before Log class has loaded (advanced internal usage)
isc.Log._logPrelogs();

// capture a stack trace for every JS error.  
//

isc.Log.supportsOnError = ( 
    isc.Browser.isIE  && (isc.Browser.version <= 9  || isc.Browser.version >= 11) ||
    isc.Browser.isMoz &&  isc.Browser.version >= 31
);

isc.Log.fallThroughToOnError = isc.Browser.isChrome && isc.Browser.version >= 34;

if ((isc.Log.supportsOnError || isc.Log.fallThroughToOnError) &&
    !(window.isc_installOnError == false))
{
    
    window.onerror = function (msg, file, lineNo, columnNo, error) {

        if (error != null) {
            // if an error object is present, use it to report
            isc.Log._reportJSError(error);
        } else {
            // arguments.caller is deprecated, equivalent of arguments.callee.caller.arguments
            // (See getStackTrace implementation for more on how we work with errors)
            //
            // Note: 
            // - In FF 3.6+ while onerror fires, it appears we can't walk the stack -
            //   arguments.callee.caller is always null at this point.
            // - In IE9, we also can't walk the stack, though we can identify the function where
            //   the crash occurred (but not its arguments)
            //   - this is because arguments.callee.caller is there, but
            //     arguments.callee.caller.arguments is not, nor arguments.callee.caller.caller
            // Note in both cases the thread in onerror is actually running in some kind of 
            // special security context: it's not just that you can't traverse through the
            // onerror function, even if you know the name of a function in the stack
            // beforehand, accessing func.caller on that function is null.
            var callerArgs = arguments.caller,
            caller;
            if (callerArgs == null && arguments.callee.caller != null) {
                caller = arguments.callee.caller;
                callerArgs = caller.arguments;
            }

            // one-time flag to avoid doubled reports for errors that are caught, go through
            // _reportJSError(), and are rethrown
            if (callerArgs && callerArgs._errorReported) {
                return;
            }
            
            var message = "Error:\r\t'" + msg + "'\r\tin " + file + "\r\tat line " + lineNo;

            // in IE9 called from window.onerror, this is the way the stacks end (not with a
            // bang but a whimper).  We can at least log the name of the function that crashed
            // and direct users to other browsers for better diagnostics.
            // Note we can't check Browser.isIE9 because that's enabled only when IE9 is not
            // running in a compatibility mode to emulate other browser's rendering.  This
            // JavaScript behavior is present in all modes.
            if (caller != null && callerArgs == null && 
                isc.Browser.isIE && isc.Browser.version >= 9)
            {
                message += "\r\n    crashed in:  " + isc.Func.getName(caller, true) + "()" +
                    "\r\n    Use a pre-9.0 Internet Explorer for best diagnostics, " +
                    "otherwise Firefox or Chrome";
            } else if (callerArgs != null) {
                message += isc.Log.getStackTrace(callerArgs);
            }
        
            isc.Log.logWarn(message);
        }

        if (isc.Browser.isIE && isc.useIEDebugger) {
            if (confirm("Run debugger?" + "\r\r" + message)) {
                debugger;
            }
        }
    };
} else if (isc.Browser.autotest == isc.Browser.RUNNER) {
    
    window.onerror = function (msg, url, line) {
        if (isc.TestRunner) {
            isc.TestRunner.addUnassignedErrorDetails("Javascript Exception at " + url +
                                                     ", line " + line + ": " + msg);
        }
    };
}



// shared toString method for data model classes (ResultSet, ResultTree)
isc._dataModelToString = function () {
// Add in a check for a DataSource and if present, record the DS Name as part of the description
    var ds = this.getDataSource();
        if (ds && ds.ID) {
            return "[" + this.Class + " ID:" + this.ID + 
            (this.componentId != null ? " (dataSource: " + ds.ID + ", created by: " + this.componentId + ")" 
                                      : "(dataSource: " + ds.ID + ", created directly)") + 
            "]"; 
        } else {
            return "[" + this.Class + " ID:" + this.ID + 
            (this.componentId != null ? " (created by: " + this.componentId + ")" 
                                      : "(created directly)") + 
            "]";
        }
}

// shared logMessage method for data model classes (Resultset, ResultTree)
isc._dataModelLogMessage = function (priority, message, category, timestamp) {
    var log = isc.Log;
    if (!log) return;

    //>DEBUG
    
    // if no priority was passed in, use the default
    if (priority == null) priority = log.defaultPriority;
    
    // automatically add a stack trace for error logs
    if (priority <= log.stackTracePriority && this.getStackTrace != null) {
        // skip two levels of the stack to avoid showing the logMessage() invocation itself 
        message += "\nStack trace:\n" + this.getStackTrace(arguments, 2);
    }

    // If a category was not specified, use the name of this class.
    if (!category) category = this.Class;

    // actually do the log.  NOTE: if we have an instance ID, pass it
     // Add in a check for a DataSource and if present, record the DS Name as part of the description
    var ds = this.getDataSource();
    if (ds && ds.ID) {
        log.log(priority, message, category, this.ID  + " (dataSource: " + ds.ID + ", created by: " + this.componentId + ")",
             this, timestamp);  
    } else {
        log.log(priority, message, category, this.ID  + " (created by: " + this.componentId + ")",
            this, timestamp);
    }
    
    //<DEBUG
}




