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
// Class will not work without the ListGrid
if (isc.ListGrid) {





//> @class PresetCriteriaItem
// A FormItem for use with the +link{FilterBuilder}, allows the user to pick from a set of 
// pre-configured search criteria such as specific ranges in numeric or date data, and provide 
// user friendly titles for such criteria, such as "within the next two weeks" or 
// "High (0.75 - 0.99)".
// 
// @visibility external
//<
isc.defineClass("PresetCriteriaItem", "SelectItem");

isc.PresetCriteriaItem.addClassMethods({

});


isc.PresetCriteriaItem.addProperties({
    height: 20,
    cellHeight: 20,
    canFocus: true,

    //> @attr presetCriteriaItem.valueMap (Object : null : IR)
    // This attribute is not applicable to the PresetCriteriaItem.  See 
    // +link{presetCriteriaItem.options} instead.
    // 
    // @visibility external
    //<

    //> @attr presetCriteriaItem.options (Object : null : IR)
    // An object whose properties are user-visible titles for the preset ranges, and whose 
    // values are Criteria / AdvancedCriteria objects representing the criteria to be used if 
    // the user selects that choice.
    // 
    // @visibility external
    //<

    // i18n attributes

    //> @attr presetCriteriaItem.showCustomOption (boolean : false : IR)
    // If set, an additional option will be shown with the title +link{customOptionTitle}, 
    // which will cause +link{getCustomCriteria()} to be called.
    // 
    // @visibility external
    //<

    //> @attr presetCriteriaItem.customOptionTitle (String : "Custom..." : IR)
    // The title to show for the +link{presetCriteriaItem.showCustomOption, custom option}.
    //
    // @visibility external
    // @group i18nMessages
    //<
    customOptionTitle: "Custom...",

    //> @attr presetCriteriaItem.shouldSaveValue (Boolean : true : IR)
    // @include FormItem.shouldSaveValue
    //<
    shouldSaveValue: true

});

isc.PresetCriteriaItem.addMethods({
    
    init : function () {
        if (!this.options) this.options = {};

        this.valueMap = {};
        for (var key in this.options) {
            this.valueMap[key] = key;
        }

        if (this.showCustomOption) {
            this.options["customOption"] = null;
            this.valueMap["customOption"] = this.customOptionTitle; 
        }

        this.Super("init", arguments);
    },

    changed : function (form, item, value) {
        //this.Super("changed", arguments);
        if (value == "customOption") {
            if (this.getCustomCriteria && isc.isA.Function(this.getCustomCriteria))
                this.getCustomCriteria(this.getID()+".getCustomCriteriaReply(criteria,title)");
        }
    }
    
    ,

    //> @method presetCriteriaItem.getCustomCriteria() [A]
    // This method is called when +link{showCustomOption} is true and the user selects the 
    // custom option.  Implement this method by allowing the user to enter custom criteria, for
    // example, by opening a modal dialog.  Once the user has input customer criteria, fire the
    // callback method with the resulting criteria.
    // 
    // @param callback (Callback) callback to fire when custom criteria has been gathered.
    //              Expects parameters "criteria,title".  The "title" will be displayed as the 
    //              currently selected value when custom criteria have been chosen.
    // 
    // @visibility external
    //<
    getCustomCriteria : function (callback) {
    },

    getCustomCriteriaReply : function (criteria, title) {
        this.valueMap["customOption"] = title;
        this.options["customOption"] = criteria;
    },

    getCriteriaValue : function () {
        return this.getCriterion();
    },

    hasAdvancedCriteria : function () {
        return true;
    }

    ,

    //> @method presetCriteriaItem.getCriterion()
    // Get the criterion based on the value selected by the user.
    //
    // @return (Criterion or AdvancedCriteria) the criteria for the selected option
    // 
    // @visibility external
    //<
    getCriterion : function () {
        var key = this.getValue(),
            criterion = this.options[key]
        ;

        return criterion;
    },

    updateCriteriaFieldNames : function (crit) {
        for (var key in crit) {
            var subCrit = crit[key];

            if (isc.DataSource.isAdvancedCriteria(subCrit)) {
                subCrit = this.updateCriteriaFieldNames(subCrit);
            } else {
                if (!subCrit.fieldName) {
                    subCrit.fieldName = this.getCriteriaFieldName();
                }
            }
        }

        return crit;
    },
    
    setValue : function (value) {
        value = this.matchCriteria(value);
        this.Super("setValue", arguments);
    },
    
    setValueMap : function (valueMap) {
        // valueMap is not supported for this FormItem type - we use this.options instead - 
        // ignore this call
        return null;
    },
    
    canEditCriterion : function (criterion) {
        var fieldNames = isc.DS.isAdvancedCriteria(criterion) ?
                isc.DS.getCriteriaFields(criterion).getUniqueItems() :
                criterion.criteria ? criterion.criteria.getProperty("fieldName").getUniqueItems() :
                [criterion.fieldName]
        ;
        
        var fieldName = this.getCriteriaFieldName();
        return fieldNames.contains(fieldName);
    },

    setCriterion : function (criterion) {
        this.setValue(isc.shallowClone(criterion));
    },

    // find the entry in this.options that maps to the criteria object passed in
    matchCriteria : function (criteria, passedOptions) {
        var options = passedOptions || this.options;

        for (var key in options) {
            var option = options[key];

            if (this.objectsAreEqual(option, criteria)) {
                return key;
            }
        }

        return criteria;
    },

    // helper method to compare the properties on two objects, not dissimilar to 
    // DS.compareCriteria
    objectsAreEqual : function (object1, object2) {
        if (!object1 && !object2) return true;
        if (!object1 || !object2) return false;

        for (var key in object1) {
            if (key == "_constructor") continue;

            var prop1 = object1[key],
                prop2 = object2[key]
            ;

            if (isc.isAn.Array(prop1)) {
                for (var i=0; i<prop1.length; i++) {
                    if (isc.isAn.Object(prop1[i])) {
                        if (!this.objectsAreEqual(prop1[i], prop2[i])) return false;
                    } else {
                        if (prop1[i] != prop2[i]) return false;
                    }
                }
            } else if (isc.isAn.Object(prop1)) {
                if (!this.objectsAreEqual(prop1, prop2)) return false;
            } else {
                if (object1[key] != object2[key]) return false;
            }
        }
        return true;
    }


});

//> @class PresetDateRangeItem
// Allows the user to pick from pre-set date ranges or choose a custom date range via a
// +link{DateRangeDialog}.
// <P>
// To use this item in the +link{listGrid.showFilterEditor,FilterEditor} or 
// +link{FilterBuilder}, create a trivial +link{ClassFactory.defineClass,subclass} which 
// defines +link{presetCriteriaItem.options,preset options}, then set
// +link{listGridField.filterEditorType} to use this class with the FilterEditor, or define a
// custom operator and set +link{operator.editorType} to use it with the FilterBuilder.
// <P>
// See the +explorerExample{dateRangeFilterPresets,Date Range (Presets)} example for sample code.
// @visibility external
//<
isc.defineClass("PresetDateRangeItem", "PresetCriteriaItem");

isc.PresetDateRangeItem.addProperties({

    customOptionTitle: "Custom Date Range",

    getCustomCriteria : function (callback) {
        this._callback = callback;
        isc.DateRangeDialog.askForRange(
            true, { returnCriterion: true }, null, this.getID()+".showDateRangeDialogReply(value)"
        );
    },

    showDateRangeDialogReply : function (value) {
        var callback = this._callback;
        delete this._callback;
        this.fireCallback(callback, "criteria,title", [value,this.customOptionTitle]);
    }

    //> @method presetDateRangeItem.getCriterion()
    // Get the criterion based on the value selected by the user.
    //
    // @return (Criterion or AdvancedCriteria) the criteria for the selected option
    // 
    // @visibility external
    //<
    , getCriterion : function () {
        var key = this.getValue(),
            criterion = this.options[key]
        ;
	return criterion;
    }
});

}
