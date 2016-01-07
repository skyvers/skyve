/*
 * Isomorphic SmartClient
 * Version SNAPSHOT_v10.1p_2015-12-10 (2015-12-10)
 * Copyright(c) 1998 and beyond Isomorphic Software, Inc. All rights reserved.
 * "SmartClient" is a trademark of Isomorphic Software, Inc.
 *
 * licensing@smartclient.com
 *
 * http://smartclient.com/license
 */
isc.defineClass("QuartzManager", "SectionStack").addProperties({

visibilityMode: "multiple",


jobsPauseBtnDefaults: {
    _constructor: "IButton", 
    title: "Pause Job",
    prompt: "Suspends all triggers associated with selected job",
    click : function () {
        var jobsGrid = this.creator.jobsGrid;
        if (!jobsGrid.anySelected()) {
            isc.say("Please select a job first");
            return;
        }
        var job = jobsGrid.getSelectedRecord();        
        var _this = this;
        QuartzJobs.performCustomOperation("pauseJob", {group: job.group, name: job.name}, function (dsResponse) {
            _this.creator.triggersGrid.invalidateCache();
            isc.say('Job Paused');
        });
    }
},

jobsResumeBtnDefaults: {
    _constructor: "IButton", 
    title: "Resume Job",
    prompt: "Resumes all triggers associated with selected job",
    click : function () {
        var jobsGrid = this.creator.jobsGrid;
        if (!jobsGrid.anySelected()) {
            isc.say("Please select a job first");
            return;
        }
        var job = jobsGrid.getSelectedRecord();        
        var _this = this;
        QuartzJobs.performCustomOperation("resumeJob", {group: job.group, name: job.name}, function (dsResponse) {
            _this.creator.triggersGrid.invalidateCache();
            isc.say('Job Resumed');
        });
    }
},


jobsTriggerBtnDefaults: {
    _constructor: "IButton", 
    title: "Trigger Job",
    prompt: "Triggers selected job immediately",
    click : function () {
        var jobsGrid = this.creator.jobsGrid;
        if (!jobsGrid.anySelected()) {
            isc.say("Please select a job first");
            return;
        }
        var job = jobsGrid.getSelectedRecord();        
        QuartzJobs.performCustomOperation("triggerJob", {group: job.group, name: job.name}, function (dsResponse) {
            isc.say('Job Triggered');
        });
    }
},

jobsRefreshBtnDefaults: {
    _constructor: "ImgButton", 
	showRollOver: false,
    size: 16,
	src: "[SKIN]actions/refresh.png",
	prompt: "Refresh jobs",
    click : function () {
        this.creator.jobsGrid.invalidateCache();
        this.creator.triggersGrid.setData([]);
    }
},

jobsAddBtnDefaults: {
	_constructor: "ImgButton",
    size: 16,
	showRollOver: false,
	src: "[SKIN]actions/add.png",
	prompt: "Add job",
	click: "this.creator.jobsGrid.startEditingNew()"
},

jobsRemoveBtnDefaults: {
	_constructor: "ImgButton",
    size: 16,
	showRollOver: false,
	src: "[SKIN]actions/remove.png",
	prompt: "Remove job",
	click: function () {
        var _this = this;
        isc.ask("Are you sure you wish to delete the selected job?  This will remove all"
          + " triggers associated with this job.", function (yes) {
             if (yes) _this.creator.jobsGrid.removeSelectedData(function (dsResponse) {
                 _this.creator.triggersGrid.setData([]);
             });
        });
    }
},

jobsGridDefaults: {
	_constructor: "ListGrid",
	autoDraw: false,
	height: "30%",
	dataSource: "QuartzJobs",
	useAllDataSourceFields: true,
	autoFetchData: true,
	selectionType: "single",
    recordDoubleClick : function () {
        isc.say("The Quartz APIs do not allow modification of job metadata without destroying"
          + " all triggers attached to the job, so you must remove and re-create the job if"
          + " that's your intention");
        return;
    },
    selectionChanged : function (record, state) {
        if (state) {
            this.creator.triggersGrid.filterData({
                jobGroup: record.group,
                jobName: record.name
            });
        } else {
            this.creator.triggersGrid.setData([]);
        }
    },
	remove : function() {
	}
},




triggersPauseBtnDefaults: {
    _constructor: "IButton", 
    title: "Pause Trigger",
    prompt: "Suspends selected trigger",
    click : function () {
        var triggersGrid = this.creator.triggersGrid;
        if (!triggersGrid.anySelected()) {
            isc.say("Please select a trigger first");
            return;
        }
        var trigger = triggersGrid.getSelectedRecord();        
        QuartzTriggers.performCustomOperation("pauseTrigger", {group: trigger.group, name: trigger.name}, function (dsResponse) {
            triggersGrid.invalidateCache();
            isc.say('Trigger Paused');
        });
    }
},

triggersResumeBtnDefaults: {
    _constructor: "IButton", 
    title: "Resume Trigger",
    prompt: "Resumes selected trigger",
    click : function () {
        var triggersGrid = this.creator.triggersGrid;
        if (!triggersGrid.anySelected()) {
            isc.say("Please select a trigger first");
            return;
        }
        var trigger = triggersGrid.getSelectedRecord();          
        QuartzTriggers.performCustomOperation("resumeTrigger", {group: trigger.group, name: trigger.name}, function (dsResponse) {
            triggersGrid.invalidateCache();
            isc.say('Trigger Resumed');
        });
    }
},

triggersRefreshBtnDefaults: {
    _constructor: "ImgButton", 
	showRollOver: false,
    size: 16,
	src: "[SKIN]actions/refresh.png",
	prompt: "Refresh jobs",
	click: "this.creator.triggersGrid.invalidateCache()"
},

triggersAddBtnDefaults: {
	_constructor: "ImgButton",
    size: 16,
	showRollOver: false,
	src: "[SKIN]actions/add.png",
	prompt: "Add trigger",
    click : function () {
        var jobsGrid = this.creator.jobsGrid;
        if (!jobsGrid.anySelected()) {
            isc.say("Please select a job first");
            return;
        }
        var job = jobsGrid.getSelectedRecord();
        this.creator.triggersGrid.startEditingNew({
            jobGroup: job.group,
            jobName: job.name
        });
    }
},

triggersRemoveBtnDefaults: {
	_constructor: "ImgButton",
    size: 16,
	showRollOver: false,
	src: "[SKIN]actions/remove.png",
	prompt: "Remove job",
	click: function () {
        var _this = this;
        isc.ask("Are you sure you wish to remove the selected trigger?", function (yes) {
             if (yes) _this.creator.jobsGrid.removeSelectedData(function (dsResponse) {
                 _this.creator.triggersGrid.invalidateCache();
             });
        });
    }
},

triggersGridDefaults: {
	_constructor: "ListGrid", 
    canEdit: true,
	autoDraw: false,
	dataSource: "QuartzTriggers",
	useAllDataSourceFields: true,
	selectionType: "single",
	remove : function() {
	}
},


initWidget : function () {
    this.Super("initWidget", arguments);

    this.jobsPauseBtn = this.createAutoChild("jobsPauseBtn");
    this.jobsResumeBtn = this.createAutoChild("jobsResumeBtn");
    this.jobsTriggerBtn = this.createAutoChild("jobsTriggerBtn");
    this.jobsRefreshBtn = this.createAutoChild("jobsRefreshBtn");
    this.jobsAddBtn = this.createAutoChild("jobsAddBtn");
    this.jobsRemoveBtn = this.createAutoChild("jobsRemoveBtn");

    // This allows us to pass an override autoFetchData flag through. We use this on the admin
    // console in order to control when the data should be loaded as we don't want it to load
    // when the component is drawn.
    this.jobsGrid = this.createAutoChild("jobsGrid", { autoFetchData: this.autoFetchData !== false });

    this.addSection({
        title: "Jobs", 
        expanded: true, 
        items: [this.jobsGrid],
        controls: [this.jobsPauseBtn, this.jobsResumeBtn, this.jobsTriggerBtn, this.jobsRefreshBtn, this.jobsAddBtn, this.jobsRemoveBtn]
    });

    this.triggersPauseBtn = this.createAutoChild("triggersPauseBtn");    
    this.triggersResumeBtn = this.createAutoChild("triggersResumeBtn");    
    this.triggersRefreshBtn = this.createAutoChild("triggersRefreshBtn");    
    this.triggersAddBtn = this.createAutoChild("triggersAddBtn");    
    this.triggersRemoveBtn = this.createAutoChild("triggersRemoveBtn");    

    this.triggersGrid = this.createAutoChild("triggersGrid");

    this.addSection({
        title: "Triggers", 
        expanded: true, 
        items: [this.triggersGrid],
        controls: [this.triggersPauseBtn, this.triggersResumeBtn, this.triggersRefreshBtn, this.triggersAddBtn, this.triggersRemoveBtn]
    });
}

});
