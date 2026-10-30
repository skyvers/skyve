<script>
import Column from 'primevue/column';
import { FilterOperator } from 'primevue/api';
import { MatchModes } from './support/MatchModes';
import Dropdown from 'primevue/dropdown';
import { openDocInNewWindow, openDocInSameWindow, SNAP_KEY_PREFIX, unsanitiseBinding } from './support/Util';
import { applyConverters } from './support/Converters';

/**
 * Map from the skyve attribute type to
 * the default filter operator to use for 
 * that column.
 */
function defaultMatchMode(columnType) {

    return (MatchModes[columnType] ?? [MatchModes.MODES.EQUALS])[0].value;
}

/**
 * Compare the two provided arrays for equality;
 * ie: same length, and contents are equal 
 * accoring to ==.
 */
function arraysEqual(a, b) {

    if (!Array.isArray(a) || !Array.isArray(b)) {
        return false;
    }

    if (a.length != b.length) {
        return false;
    }

    return a.every((val, index) => val == b[index])
}

/**
 * Call the provided RemoteCommand function, converting the 
 * supplied parameters object from `{ k: v }` to 
 * `[{ name: k, value: v }]`.
 * 
 * Returns the result of the provided command (presumably a 
 * Promise)
 * 
 * @param {String} commandName The name of the function (in 
 * global scope) that will be called.
 * @param {Object} paramsObj 
 */
async function callRemoteCommand(commandName, paramsObj) {

    const commandFn = window[commandName];
    if (!commandFn) {
        throw `CommandName (${commandName}) not found`;
    }

    let paramsArray = [];
    for (let key in paramsObj) {
        paramsArray.push({ name: key, value: paramsObj[key] });
    }

    return commandFn(paramsArray);
}

/**
 * Scroll driven animations move the sticky header in step with the page scroll (no lag).
 * Otherwise the header is moved by a scroll event listener.
 */
const SCROLL_DRIVEN_ANIMATIONS = (typeof CSS !== 'undefined') && !!CSS.supports?.('animation-timeline: view()');

export default {
    props: {
        owningModule: String,
        owningDocument: String,
        drivingModule: String,
        drivingDocument: String,
        query: String,
        model: String,
        title: String,
        columns: Object,
        dateFormat: {
            type: String,
            default: 'dd/mm/yy'
        },
        dateTimeFormat: {
            type: String,
            default: 'dd/mm/yy'
        },
        hourFormat: {
            type: String,
            default: '24'
        },
        contextId: {
            type: String,
            default: null
        },
        // The (sanitised) view binding when the grid is in a zoomed in view
        contextBinding: {
            type: String,
            default: null
        },
        // Filter parameters ({name, operator, value}) and parameters (name starts with ':')
        // from the view's listGrid definition, with their values resolved on the server
        params: {
            type: Array,
            default: () => []
        },
        // Request parameters used to populate a new document created from this grid
        newParams: {
            type: Object,
            default: () => ({})
        },
        actions: {
            type: Object,
            default: {
                selected: null,
                edited: null,
                deleted: null
            }
		},
        showAdd: {
            type: Boolean,
            default: true
        },
        showZoom: {
            type: Boolean,
            default: true
        },
        // Show the View Detail/Popout Detail row context menu
        showContextMenu: {
            type: Boolean,
            default: true
        },
        // Disable conditions evaluated on the server
        disableAdd: {
            type: Boolean,
            default: false
        },
        disableZoom: {
            type: Boolean,
            default: false
        },
        showFilter: {
            type: Boolean,
            default: true
        },
        showSummary: {
            type: Boolean,
            default: true
        },
        showSnap: {
            type: Boolean,
            default: true
        },
        stickyHeaderAnchorSelector: {
            type: String,
            default: null
        },
        // Changes each time the grid's container is rendered on the server (see main.js)
        renderedAt: {
            type: Number,
            default: 0
        }
    },
    data() {
        return {
            loading: true,
            loadTimeout: null,
            value: [],
            totalRecords: 0,
            filters: {},
            smartClientCriteria: null,
            firstRow: 0,
            pageSize: 50,
            errorText: '',

            multiSortMeta: [],

            selectedColumns: [],
            columnOrder: [],
            columnWidths: [],

            summarySelection: '',
            summaryOpts: ['', 'Count', 'Avg', 'Sum', 'Min', 'Max'],
            summaryRow: {},

            topLevelOperators: [
                {
                    label: 'Match All',
                    value: 'and'
                },
                {
                    label: 'Match Any',
                    value: 'or'
                }
            ],
            selectedTopLevelOperator: 'and',

            matchModes: MatchModes,

            // Support params for row right click context menu:
            selectedRow: null,
            // The row right clicked for the context menu (separate from the selection)
            contextMenuRow: null,
            stickyHeaderTop: 0,
            stickyHeaderRefreshTimeout: null,
            stickyHeaderFrame: null,
            // Paginator layout - 'centred', 'uncentred' or 'stacked' (see layoutPaginator())
            pagerLayout: 'centred',
            pagerObserver: null,
            // The last full size (not compact) paginator measurements
            pagerWidths: null,
            pagerFrame: null,
            // The header toolbar controls are stacked (see layoutToolbar())
            toolbarStacked: false,
            toolbarObserver: null,
            toolbarFrame: null,
            // Reload without the loading overlay (keeping the current rows until the new ones arrive)
            quietLoad: false,
            // A fetch is pending or in flight (shows the spinner in the paginator row and hides the
            // floating New button) - true to start with as the grid loads when it is mounted
            fetching: true,
            // The horizontal scroll position, restored when the grid is put back after a re-render
            scrollLeft: 0,
            stickyHeaderMenuRefreshHandler: null,
            menuModel: [
                {
                    label: 'View Detail',
                    icon: 'pi pi-angle-right',
                    disabled: () => this.disableZoom,
                    command: () => this.highlightRow(this.contextMenuRow) && openDocInSameWindow({
                        bizId: this.contextMenuRow.bizId,
                        module: this.contextMenuRow.bizModule,
                        document: this.contextMenuRow.bizDocument
                    })
                },
                {
                    label: 'Popout Detail',
                    icon: 'pi pi-external-link',
                    disabled: () => this.disableZoom,
                    command: () => this.highlightRow(this.contextMenuRow) && openDocInNewWindow({
                        bizId: this.contextMenuRow.bizId,
                        module: this.contextMenuRow.bizModule,
                        document: this.contextMenuRow.bizDocument
                    })
                }
            ],
        };
    },
    computed: {
        endRow() {
            return this.firstRow + this.pageSize;
        },
        /**
         * A map of the column definitions keyed 
         * on the 'field' property.
         * 
         * Additionally calculate a dataType property
         * used to determine which filter operators
         * are shown.
         */
        columnDefinitionsMap() {

            // Map from the type to 'dataType'
            // LHS: skyve attribute type
            // RHS: the dataType value on the Column, determines the 
            // comparison operators available
            const columnDataTypesMap = {
                boolean: 'boolean',
                numeric: 'numeric',
                date: 'date',
                dateTime: 'date',
                timestamp: 'date',
                time: 'date',
                enum: 'text'
            };

            const columnMap = new Map();
            this.columns.forEach(columnDefinition => {

                const defCopy = Object.assign({}, columnDefinition);

                // Modify properties as needed
                // Default type to 'text' if not mapped above
                const dataType = columnDataTypesMap[defCopy.type] ?? 'text'
                defCopy.dataType = dataType;

                columnMap.set(defCopy.field, defCopy);
            });

            return columnMap;
        },
        visibleColumns() {

            // Calculate which columns are visible
            let showPredicate;
            if (this.selectedColumns == null || this.selectedColumns.length == 0) {
                // All columns that are not hidden by default if nothing is chosen
                showPredicate = (col) => !col.hidden;
            } else {
                // Or only the selected column
                const shownColumns = this.selectedColumns.map(sc => sc.field);
                showPredicate = (col) => shownColumns.includes(col.field);
            }

            // Removing hidden columns
            const visCols = [...this.columnDefinitionsMap.values()].filter(showPredicate);

            if (this.columnOrder.length > 0) {
                // Sort the columns usin g
                visCols.sort((a, b) => {

                    const aPosn = this.columnOrder.indexOf(a.field);
                    const bPosn = this.columnOrder.indexOf(b.field);
                    if (aPosn == -1 && bPosn == -1) {
                        // Neither column appears in the DataTable's
                        // columnOrder
                        return 0;
                    }

                    if (aPosn == -1) {
                        // Put 'a' after 'b'
                        return 1;
                    }

                    if (bPosn == -1) {
                        // Put 'a' before 'b'
                        return -1;
                    }

                    return aPosn - bPosn;
                });
            }

            return visCols;
        },
        dataSource() {
            if (this.query) {
                return `${this.owningModule}_${this.query}`;
            } else if (this.model) {
                return `${this.owningModule}_${this.owningDocument}__${this.model}`;
            } else {
                return `${this.owningModule}_${this.drivingDocument}`;
            }
        },
        fetchFormData() {
            // Constuct the FormData object that will be POSTed

            const fd = new FormData();
            fd.append('_operationType', 'fetch');
            fd.append('_dataSource', this.dataSource);
            fd.append('_startRow', this.firstRow);
            fd.append('_endRow', this.endRow);

            if (!!this.summarySelection) {
                fd.append('_summary', this.summarySelection);
            }

            if (!!this.contextId) {
                fd.append('_c', this.contextId);
                if (!!this.contextBinding) {
                    fd.append('_b', this.contextBinding);
                }
            }

            for (let sortCol of this.sortColumns) {
                fd.append('_sortBy', sortCol);
            }

            // Parameters are sent as ':name=value' request parameters
            const filterParams = [];
            for (let param of this.params) {
                if (param.name.startsWith(':')) {
                    fd.append(param.name, param.value ?? '');
                }
                else {
                    filterParams.push(param);
                }
            }

            if (filterParams.length > 0) {
                // Filter parameters must always apply, so 'and' them with
                // the user's criteria (which may be 'or'ed together)
                fd.append('_constructor', 'AdvancedCriteria');
                fd.append('operator', 'and');

                for (let param of filterParams) {
                    fd.append('criteria', JSON.stringify({
                        fieldName: param.name,
                        operator: param.operator,
                        value: param.value
                    }));
                }

                if (!! this.smartClientCriteria) {
                    fd.append('criteria', JSON.stringify(this.smartClientCriteria));
                }
                else if (this.skyveCriteria.length > 0) {
                    fd.append('criteria', JSON.stringify({
                        _constructor: 'AdvancedCriteria',
                        operator: this.selectedTopLevelOperator,
                        criteria: this.skyveCriteria
                    }));
                }
            }
            else if (!! this.smartClientCriteria) {
                fd.append('criteria', JSON.stringify(this.smartClientCriteria));
            }
            else if (this.skyveCriteria.length > 0) {
                fd.append('_constructor', 'AdvancedCriteria');
                fd.append('operator', this.selectedTopLevelOperator);

                for (let crit of this.skyveCriteria) {
                    fd.append('criteria', JSON.stringify(crit));
                }
            }

            return fd;
        },
        sortColumns: {
            get() {
                // convert from the DataTable's format eg:
                // [ { "field": "longInteger", "order": 1 }, { "field": "normalInteger", "order": -1 } ]
                // to what we'll send, eg:
                // [ "longInteger", "-normalInteger" ]
                return this.multiSortMeta
                    .map(col => `${col.order == 1 ? '' : '-'}${col.field}`);
            },
            set(newValue) {
                // reverse of the above
                this.multiSortMeta = newValue.map(str => ({
                    field: str.replace(/^-/, ''),
                    order: str.charAt(0) == '-' ? -1 : 1
                }));
            }
        },
        skyveCriteria() {
            // Convert from the DataTable's 'filter' property
            // to something we can send to Skyve

            let criteria = [];

            for (let columnFilter of Object.entries(this.filters)) {

                const columnName = columnFilter[0];
                const { operator, constraints } = columnFilter[1];

                // Ignore contstraints with empty/nullish value
                const nonNullConstraints = constraints.filter(con => (con.value ?? '') !== '');

                const createCriteria = (constraint) => ({
                    'fieldName': columnName,
                    'value': constraint.value,
                    'operator': constraint.matchMode
                });

                if (nonNullConstraints.length == 1) {
                    // One constraint for this column
                    const crit = createCriteria(nonNullConstraints[0])
                    criteria.push(crit);
                } else if (nonNullConstraints.length > 1) {
                    // Multiple constraints for this column

                    const groupCriteria = {
                        "_constructor": "AdvancedCriteria",
                        "operator": operator,
                        "criteria": []
                    };

                    // Create one criteria for each the user entered
                    // and smush them together into `groupCriteria`
                    nonNullConstraints
                        .map(createCriteria)
                        .forEach(c => groupCriteria.criteria.push(c));

                    criteria.push(groupCriteria);
                }
            }

            return criteria;
        },
        snapshotState() {

            const visibleColNames = this.selectedColumns.map(col => col.field);

            return {
                "filters": this.filters,
                "smartClientCriteria": this.smartClientCriteria,
				"operator": this.selectedTopLevelOperator,
                "visibleColumns": visibleColNames,
                "summarySelection": this.summarySelection,
                "sortColumns": this.sortColumns,
                "columnWidths": this.columnWidths
            };
        },
        /**
         * Whether the sticky header is moved by a scroll driven animation (otherwise by a scroll listener).
         */
        scrollDriven() {
            return SCROLL_DRIVEN_ANIMATIONS;
        },
        /**
         * The list grid has a selectedIdBinding (a selected action) - selecting a row sets it and
         * clicking the selected row again unselects it (clearing it).
         * Otherwise selection just highlights the row last clicked (eg the one zoomed into) and
         * clicking it again keeps it selected (metaKeySelection).
         */
        selectable() {
            return !!this.actions?.selected;
        },
        /**
         * Without a selected action, clicking a row zooms into it (if zoom is allowed).
         */
        zoomOnRowClick() {
            return !this.selectable && this.showZoom && !this.disableZoom;
        },
        /**
         * The compact paginator shows the current page of the total instead of the page links.
         */
        pagerTemplate() {
            return (this.pagerLayout === 'compact') ?
                        'PrevPageLink CurrentPageReport NextPageLink RowsPerPageDropdown' :
                        'FirstPageLink PrevPageLink PageLinks NextPageLink LastPageLink RowsPerPageDropdown';
        },
        canAdd() {
            return this.showAdd && !this.disableAdd;
        },

    },
    methods: {

        notUsed() {
            // Some dumb nonsense to prevent the transpilation process
            // from using $ as a variable name and blatting jQuery
            // There's potentially other globals at risk like this too
            const dontDelete = $;
        },
        /**
         * Call load after a slight delay, cancelling any pending
         * calls.
         */
        debouncedLoad() {
            this.fetching = true;
            if (!this.quietLoad) {
                this.loading = true;
            }
            const delay = 250;
            if (this.loadTimeout) {
                clearTimeout(this.loadTimeout);
            }
            this.loadTimeout = setTimeout(() => {
                this.load();
                this.loadTimeout = null;
            }, delay);
        },
        /**
         * Load the rows, keeping the spinner going until the load finishes (or fails).
         */
        async load() {
            try {
                await this.loadRows();
            }
            finally {
                // Only stop the spinner if another load hasn't been scheduled in the meantime
                if (!this.loadTimeout) {
                    this.fetching = false;
                }
            }
        },
        async loadRows() {

            const listRequest = new Request('./smartlist', {
                method: 'POST',
                body: new URLSearchParams(this.fetchFormData),
                headers: {
                    'Content-Type': 'application/x-www-form-urlencoded;charset=UTF-8'
                }
            });
            const response = await fetch(listRequest);
            let payload = await response.json();
            if (payload?.response?.status == -1) {
                const errorResponse = payload?.response?.data;
                console.error('Something went wrong retrieving list contents', payload);
                this.errorText = errorResponse;
                this.loading = false;
                this.quietLoad = false;
                throw new Error('Error loading grid contents');
            }

            this.totalRecords = payload.response.totalRows;

            const rows = payload.response.data;

            if (rows.length > 0) {

                if (this.summarySelection == "Count") {
                    rows.at(-1).skipConverters = true;
                }

                // Create column index, FIXME
                const columnDefMap = new Map();
                this.columns.forEach(def => columnDefMap.set(def.field, def));

                rows.filter(row => !row.skipConverters)
                    .forEach(row => applyConverters(row, columnDefMap));
            }

            if (!!this.summarySelection) {
                // Summary row will be the last one, set it aside
                this.summaryRow = rows.pop();
            } else {
                // Clear the summary row
                this.summaryRow = {};
            }

            this.value = rows;
            this.loading = false;
            this.quietLoad = false;
            if (this.stickyHeaderAnchorSelector) {
                this.$nextTick(this.requestStickyHeaderPosition);
            }
            // the number of page links may have changed
            this.$nextTick(this.observePaginator);
        },
        /**
         * Grab an item from storage (local or session whichever the 
         * DataTable is using).
         * 
         * @param {*} keyPrefix Prefix to add to key. Emtpy string
         * will be the DataTable's state.
         */
        getStorageItem(keyPrefix) {
            const dt = this.$refs.datatable;
            const stateKey = dt.stateKey;

            const storageLoc = dt.stateStorage == 'session' ? sessionStorage : localStorage;
            return storageLoc.getItem(keyPrefix + '' + stateKey);
        },
        setStorageItem(keyPrefix, strValue) {
            const dt = this.$refs.datatable;
            const stateKey = dt.stateKey;

            const storageLoc = dt.stateStorage == 'session' ? sessionStorage : localStorage;
            return storageLoc.setItem(keyPrefix + '' + stateKey, strValue);
        },
        stateSave(event) {
            // There doesn't appear to be any way to grab
            // these values except when the state is saved

            // Datatable's state-save may be triggered as a result 
            // of either of these changes causing reactive recursion 
            // here; using arraysEqual to avoid assigning if nothing
            // has changed.
            const newColumnOrder = event.columnOrder ?? [];
            if (!arraysEqual(newColumnOrder, this.columnOrder)) {
                this.columnOrder = newColumnOrder;
            }

            // Doco is lying about type of columnWidths
            const newWidths = event.columnWidths.split(',').map(s => Number.parseInt(s));
            if (!arraysEqual(newWidths, this.columnWidths)) {
                this.columnWidths = newWidths;
            }
        },
        stateRestore(event) {

            // Triggered when the primevue datatable restores its own state
            // copy out the props 
            this.firstRow = event.first ?? 0;
            this.pageSize = event.rows ?? 50;
            this.sortColumn = event.sortField ?? '';
            this.filters = event.filters ?? {};
            this.smartClientCriteria = event.smartClientCriteria;
        },
        /**
         * Initialise/clear the filter state, optionally setting some 
         * filter state (overlaying the provided state on the just reset state).
         */
        initFilters(incomingFilters = {}) {

            const defaultFilters = {};

            // Create a default entry in 'filters' for each column
            for (let col of this.columns) {
                defaultFilters[col.field] = {
                    operator: FilterOperator.AND,
                    constraints: [{ value: null, matchMode: defaultMatchMode(col.type) }]
                };
            }

            this.filters = Object.assign(defaultFilters, incomingFilters);
            this.smartClientCriteria = null;
        },
        snapshotChanged(newSnapshot) {

            const snapstate = newSnapshot?.snapshot;
            this.setStorageItem(SNAP_KEY_PREFIX, newSnapshot?.bizId);

            if (snapstate) {
                // Filters
                const incomingFilters = snapstate.filters ?? {};
                this.initFilters(incomingFilters);
                this.smartClientCriteria = snapstate.smartClientCriteria;

				this.selectedTopLevelOperator = snapstate.operator ?? 'and';
				
                // Visible columns
                const visibleCols = snapstate.visibleColumns ?? [];
                this.selectedColumns = [];
                for (let col of this.columns) {
                    if (visibleCols.includes(col.field)) {
                        this.selectedColumns.push(col);
                    }
                }

                // Update the DataTable's column order directly
                this.$refs.datatable.d_columnOrder = visibleCols;

                /*
                // Update the column widths?
                // DOESN'T WORK
                const columnWidths = snapstate.columnWidths ?? [];
                const colWidthString = columnWidths.join(',');
                this.$refs.datatable.columnWidthsState = colWidthString
                */

                // Summary/aggregate row
                this.summarySelection = snapstate.summarySelection ?? '';

                // Sort order and columns
                if (!!snapstate.sortColumns) {
                    this.sortColumns = snapstate.sortColumns;
                } else {
                    // Clearing whatever was set if nothing defined in snapshot
                    this.sortColumns = [];
                }

            } else {
                this.clearedSnapshot();
            }
        },
        clearedSnapshot() {

            this.selectedColumns = [];
            this.summarySelection = '';
            this.sortColumns = [];

            this.$refs.datatable.d_columnOrder = null;

            this.initFilters();
        },
        onRowContextMenu(event) {
            if (this.showContextMenu) {
                this.$refs.cm.show(event.originalEvent);
            }
        },
        /**
         * The width of a column's thumbnail images (as per the faces list grid).
         */
        imageWidth(col) {
            return col.pixelWidth ?? col.pixelHeight ?? 64;
        },
        imageHeight(col) {
            return col.pixelHeight ?? col.pixelWidth ?? 64;
        },
        /**
         * Column styles for the width and alignment defined in the query (or customisations).
         * Columns have a minimum width so that the grid scrolls horizontally instead of squashing them.
         */
        columnStyle(col) {
            const style = {};
            if (col.type == 'image') {
                // thumbnails have 5px padding either side
                style.width = style.minWidth = `${this.imageWidth(col) + 10}px`;
                style.paddingLeft = '5px';
                style.paddingRight = '5px';
            }
            else if (col.pixelWidth) {
                style.width = style.minWidth = `${col.pixelWidth}px`;
            }
            else {
                style.minWidth = '120px';
            }
            if (col.align) {
                style.textAlign = col.align;
            }
            return style;
        },
        /**
         * The URL of the content for the given row and column.
         */
        contentUrl(data, field) {
            const searchParams = new URLSearchParams();
            searchParams.append('_n', data[field]);
            searchParams.append('_doc', `${data.bizModule}.${data.bizDocument}`);
            searchParams.append('_b', unsanitiseBinding(field));
            return `content?${searchParams}`;
        },
        onRowClick(event) {
            // Ignore clicks on links and buttons within the row (eg content links)
            if (event.originalEvent?.target?.closest?.('a,button')) {
                return;
            }
            // With a selected action, selection is handled by onRowSelect() and onRowUnselect()
            if (this.zoomOnRowClick) {
                const d = event.data;
			    this.zoomInto(d.bizId, d.bizModule, d.bizDocument);
			}
        },
        /**
         * A row was selected - set the selected id binding and run the selected action (if defined).
         * Don't use the ListGrid loading style here as the view loading indicator is invoked by <remoteCommand/>.
         */
        onRowSelect(event) {
            if (this.actions.selected) {
                callRemoteCommand(this.actions.selected, {bizId: event.data.bizId});
            }
        },
        /**
         * The selected row was unselected (clicked again) - clear the selected id binding
         * (no bizId is sent) and run the selected action, as per the SmartClient list grid.
         */
        onRowUnselect() {
            if (this.actions.selected) {
                callRemoteCommand(this.actions.selected, {});
            }
        },
        /**
         * Show the row as selected (eg the row being zoomed into) without running the selected action.
         * The selection is kept in the grid's (session) state so it shows when coming back.
         * @returns true (so it can be chained)
         */
        highlightRow(row) {
            if (row && (this.selectedRow?.bizId !== row.bizId)) {
                this.selectedRow = row;
            }
            return true;
        },
        /**
         * Create a new record of the grid's driving document.
         */
        newRecord() {
            this.zoomInto(null, this.drivingModule, this.drivingDocument);
        },
        zoomInto(bizId, module, document) {
            openDocInSameWindow({
                bizId: bizId,
                module: module,
                document: document,
                // populate a new document with the filter parameter/parameter values
                params: bizId ? null : this.newParams
            });
        },
        calculateStickyHeaderTop() {
            if (!this.stickyHeaderAnchorSelector) {
                return;
            }

            let top = 0;
            for (const element of document.querySelectorAll(this.stickyHeaderAnchorSelector)) {
                top += element.getBoundingClientRect().height;
            }
            this.stickyHeaderTop = top;
        },
        scheduleStickyHeaderRefresh() {
            if (!this.stickyHeaderAnchorSelector) {
                return;
            }

            this.calculateStickyHeaderTop();
            if (this.stickyHeaderRefreshTimeout) {
                clearTimeout(this.stickyHeaderRefreshTimeout);
            }
            this.positionStickyHeader();
            this.stickyHeaderRefreshTimeout = setTimeout(() => {
                this.calculateStickyHeaderTop();
                this.positionStickyHeader();
                this.stickyHeaderRefreshTimeout = null;
            }, 250);
        },
        /**
         * The grid scrolls horizontally within its wrapper, which stops CSS sticky positioning
         * from pinning the header to the page. Instead, the header cells are moved down by the
         * distance the table has scrolled up under the anchor elements (eg the top bar).
         * With scroll driven animations, this sets the geometry the CSS animation uses
         * (it only changes when the grid or anchors change size); otherwise it sets the offset.
         */
        positionStickyHeader() {
            const root = this.$refs.datatable?.$el;
            const table = root?.querySelector('table');
            const thead = table?.tHead;
            if (!thead) {
                return;
            }

            // Tuck the header 1px under the anchor elements so that no sub-pixel seam of the rows shows through
            const top = Math.max(0, this.stickyHeaderTop - 1);
            const maxOffset = Math.max(0, table.offsetHeight - thead.offsetHeight);
            root.style.setProperty('--skyve-vue-list-grid-sticky-top', `${top}px`);
            root.style.setProperty('--skyve-vue-list-grid-sticky-max', `${maxOffset}px`);

            if (!SCROLL_DRIVEN_ANIMATIONS) {
                const scrolled = top - table.getBoundingClientRect().top;
                const offset = Math.min(Math.max(0, Math.floor(scrolled)), maxOffset);
                table.style.setProperty('--skyve-vue-list-grid-sticky-offset', `${offset}px`);
            }
        },
        /**
         * Lay out the paginator row on one line while it fits, otherwise stack it:
         * centred - the start (summary) and end (spinner) contents take equal widths so the
         *              page controls are centred in the grid.
         * uncentred - only fits on one line with the start and end contents at their own widths.
         * stacked - the start and end contents share the first row and the page controls are
         *              centred on the row below.
         */
        layoutPaginator() {
            const paginator = this.$refs.datatable?.$el?.querySelector('.p-paginator');
            if (!paginator) {
                return;
            }

            const outerWidth = (element) => {
                if (!element) {
                    return 0;
                }
                const style = getComputedStyle(element);
                return element.getBoundingClientRect().width + parseFloat(style.marginLeft) + parseFloat(style.marginRight);
            };
            const style = getComputedStyle(paginator);
            const available = paginator.clientWidth - parseFloat(style.paddingLeft) - parseFloat(style.paddingRight);

            // Measure the full size controls (the compact layout shows different controls,
            // so keep the last full size measurements to know when to switch back)
            if (this.pagerLayout !== 'compact') {
                let controls = 0;
                for (const child of paginator.children) {
                    if (!child.classList.contains('p-paginator-left-content') &&
                            !child.classList.contains('p-paginator-right-content')) {
                        controls += outerWidth(child);
                    }
                }
                this.pagerWidths = {
                    start: outerWidth(paginator.querySelector('.p-paginator-left-content')?.firstElementChild),
                    end: outerWidth(paginator.querySelector('.p-paginator-right-content')?.firstElementChild),
                    rowsPerPage: outerWidth(paginator.querySelector('.p-paginator-rpp-options')),
                    controls
                };
            }
            const { start, end, rowsPerPage, controls } = this.pagerWidths;
            const pageLinks = controls - rowsPerPage;

            // allow a pixel for rounding
            const fits = (width) => width <= (available + 1);
            if (fits(controls + (2 * Math.max(start, end)))) {
                this.pagerLayout = 'centred';
            }
            else if (fits(controls + start + end)) {
                this.pagerLayout = 'uncentred';
            }
            else if (fits(pageLinks) && fits(start + rowsPerPage + end)) {
                this.pagerLayout = 'stacked';
            }
            else {
                this.pagerLayout = 'compact';
            }
        },
        /**
         * Lay out the header toolbar controls on one row while they fit, otherwise stack them.
         * The controls are measured at their natural widths in a row (momentarily, without painting).
         */
        layoutToolbar() {
            const toolbar = this.$refs.toolbar;
            if (!toolbar) {
                return;
            }

            toolbar.classList.add('skyve-vue-list-grid-toolbar-measuring');
            const children = [...toolbar.children];
            const gap = parseFloat(getComputedStyle(toolbar).columnGap) || 0;
            let required = gap * Math.max(0, children.length - 1);
            for (const child of children) {
                const style = getComputedStyle(child);
                required += child.getBoundingClientRect().width + parseFloat(style.marginLeft) + parseFloat(style.marginRight);
            }
            const available = toolbar.clientWidth;
            toolbar.classList.remove('skyve-vue-list-grid-toolbar-measuring');

            // allow a pixel for rounding
            this.toolbarStacked = (required > (available + 1));
        },
        /**
         * Lay out the toolbar at most once per animation frame.
         */
        requestToolbarLayout() {
            if (this.toolbarFrame) {
                return;
            }
            this.toolbarFrame = requestAnimationFrame(() => {
                this.toolbarFrame = null;
                this.layoutToolbar();
            });
        },
        /**
         * Watch the toolbar and its controls for size changes (eg the grid is resized or a label changes).
         */
        observeToolbar() {
            const toolbar = this.$refs.toolbar;
            if (!toolbar || (typeof ResizeObserver === 'undefined')) {
                return;
            }
            if (!this.toolbarObserver) {
                this.toolbarObserver = new ResizeObserver(this.requestToolbarLayout);
            }
            this.toolbarObserver.disconnect();
            this.toolbarObserver.observe(toolbar);
            for (const child of toolbar.children) {
                this.toolbarObserver.observe(child);
            }
            this.requestToolbarLayout();
        },
        /**
         * Lay out the paginator at most once per animation frame.
         */
        requestPaginatorLayout() {
            if (this.pagerFrame) {
                return;
            }
            this.pagerFrame = requestAnimationFrame(() => {
                this.pagerFrame = null;
                this.layoutPaginator();
            });
        },
        /**
         * Watch the paginator and its contents for size changes (eg the grid is resized or
         * the number of page links changes) and lay it out again.
         */
        observePaginator() {
            const paginator = this.$refs.datatable?.$el?.querySelector('.p-paginator');
            if (!paginator || (typeof ResizeObserver === 'undefined')) {
                return;
            }
            if (!this.pagerObserver) {
                this.pagerObserver = new ResizeObserver(this.requestPaginatorLayout);
            }
            this.pagerObserver.disconnect();
            this.pagerObserver.observe(paginator);
            for (const child of paginator.children) {
                this.pagerObserver.observe(child);
            }
            this.requestPaginatorLayout();
        },
        /**
         * Reposition the sticky header at most once per animation frame (for scroll events).
         */
        requestStickyHeaderPosition() {
            if (this.stickyHeaderFrame) {
                return;
            }
            this.stickyHeaderFrame = requestAnimationFrame(() => {
                this.stickyHeaderFrame = null;
                this.positionStickyHeader();
            });
        }
    },
    beforeMount() {
       // Calling init filters from mounted() was
        // triggering this issue: https://github.com/primefaces/primevue/issues/4291
        // Seems like the DataTable will reset its filters to whatever
        // was set when it was mounted so we need to set the defaults earlier
        this.initFilters();
    },
    mounted() {
        // The grid loads when mounted (its filters are initialised) - make sure a load is scheduled
        // so that the initial fetching state is always cleared
        this.$nextTick(() => {
            if (this.fetching && !this.loadTimeout) {
                this.debouncedLoad();
            }
        });
        this.observePaginator();
        this.observeToolbar();
        // Remember the horizontal scroll position (it is lost when the grid is removed from the page and put back)
        this.$refs.datatable?.$el?.querySelector('.p-datatable-wrapper')?.addEventListener('scroll', (event) => {
            this.scrollLeft = event.target.scrollLeft;
        }, { passive: true });
        if (this.stickyHeaderAnchorSelector) {
            this.scheduleStickyHeaderRefresh();
            window.addEventListener('resize', this.scheduleStickyHeaderRefresh);
            window.addEventListener('orientationchange', this.scheduleStickyHeaderRefresh);
            this.stickyHeaderMenuRefreshHandler = (event) => {
                if (event.target?.closest?.('.layout-menu-button,.menu-button,#topbar-menu-button')) {
                    this.scheduleStickyHeaderRefresh();
                }
            };
            document.addEventListener('click', this.stickyHeaderMenuRefreshHandler);
            if (!SCROLL_DRIVEN_ANIMATIONS) {
                // capture so that scrolling of any scrolling ancestor is noticed, not just the window
                document.addEventListener('scroll', this.requestStickyHeaderPosition, { capture: true, passive: true });
            }
        }
    },
    beforeUnmount() {
        this.pagerObserver?.disconnect();
        this.toolbarObserver?.disconnect();
        if (this.toolbarFrame) {
            cancelAnimationFrame(this.toolbarFrame);
            this.toolbarFrame = null;
        }
        if (this.pagerFrame) {
            cancelAnimationFrame(this.pagerFrame);
            this.pagerFrame = null;
        }
        if (this.stickyHeaderRefreshTimeout) {
            clearTimeout(this.stickyHeaderRefreshTimeout);
            this.stickyHeaderRefreshTimeout = null;
        }
        window.removeEventListener('resize', this.scheduleStickyHeaderRefresh);
        window.removeEventListener('orientationchange', this.scheduleStickyHeaderRefresh);
        if (this.stickyHeaderMenuRefreshHandler) {
            document.removeEventListener('click', this.stickyHeaderMenuRefreshHandler);
            this.stickyHeaderMenuRefreshHandler = null;
        }
        document.removeEventListener('scroll', this.requestStickyHeaderPosition, { capture: true });
        if (this.stickyHeaderFrame) {
            cancelAnimationFrame(this.stickyHeaderFrame);
            this.stickyHeaderFrame = null;
        }
    },
    watch: {
        /**
         * The compact paginator shows different controls - watch the new ones.
         */
        pagerLayout() {
            this.$nextTick(this.observePaginator);
        },
        /**
         * The grid's container was re-rendered on the server (eg a faces ajax update) and this grid
         * has been put back in its place: restore the scroll position and refresh the data quietly.
         * Sync so that the quiet flag is set before any change to the request (eg parameters) reloads.
         */
        renderedAt: {
            handler() {
                this.quietLoad = true;
                this.debouncedLoad();
                this.$nextTick(() => {
                    const wrapper = this.$refs.datatable?.$el?.querySelector('.p-datatable-wrapper');
                    if (wrapper) {
                        wrapper.scrollLeft = this.scrollLeft;
                    }
                    if (this.stickyHeaderAnchorSelector) {
                        this.scheduleStickyHeaderRefresh();
                    }
                });
            },
            flush: 'sync'
        },
        fetchFormData: {
            handler(newValue, oldValue) {
                // Whenever fetchFormData changes call to server
                this.debouncedLoad();
            },
            deep: true
        }
    }
}
</script>
<template>

    <!-- Right click context menu -->
    <ContextMenu
        v-if="showContextMenu"
        ref="cm"
        :model="menuModel"
        @hide="contextMenuRow = null"
    />

    <!-- Error Dialog for any load errors -->
    <Dialog
        modal
        header="Warning"
        :visible="!!errorText"
        @update:visible="() => this.errorText = ''"
    >
        <span class="flex flex-column gap-5">
            <span class="flex flex-row gap-3">
                <div>
                    <span class="pi pi-exclamation-triangle" />
                </div>
                <div v-html="errorText" />
            </span>
            <div class="flex flex-row justify-content-center">
                <Button
                    type="button"
                    label="Ok"
                    @click="() => this.errorText = ''"
                />
            </div>
        </span>
    </Dialog>

    <div :class="['skyve-vue-list-grid', {
            'skyve-vue-list-grid-sticky': !!stickyHeaderAnchorSelector,
            'skyve-vue-list-grid-zoomable': zoomOnRowClick,
            'skyve-vue-list-grid-scroll-driven': scrollDriven,
            ['skyve-vue-list-grid-pager-' + pagerLayout]: true
        }]">
        <DataTable
            ref="datatable"
            tableStyle="min-width: 100%"
            dataKey="bizId"
            filterDisplay="menu"
            selectionMode="single"
            :metaKeySelection="!selectable"
            :rowHover="true"
            :stateKey="dataSource"
            stateStorage="session"
            :rowsPerPageOptions="[5, 10, 25, 50, 75, 100]"
            :paginatorTemplate="pagerTemplate"
            currentPageReportTemplate="{currentPage} / {totalPages}"
            :lazy="true"
            :value="value"
            :totalRecords="totalRecords"
            :paginator="true"
            :reorderableColumns="true"
            :resizableColumns="true"
            columnResizeMode="expand"
            v-model:first="firstRow"
            v-model:rows="pageSize"
            v-model:filters="filters"
            @state-restore="stateRestore"
            @state-save="stateSave"
            :contextMenu="showContextMenu"
            v-model:contextMenuSelection="contextMenuRow"
            v-model:selection="selectedRow"
            @row-contextmenu="onRowContextMenu"
            @row-click="onRowClick"
            @row-select="onRowSelect"
            @row-unselect="onRowUnselect"
            sortMode="multiple"
            v-model:multiSortMeta="multiSortMeta"
        >
            <template #header>
                <div v-if="title">
                    {{ title }}
                </div>
                <!-- One row while the controls fit, otherwise stacked - see layoutToolbar() -->
                <div ref="toolbar"
                    :class="['skyve-vue-list-grid-toolbar', { 'skyve-vue-list-grid-toolbar-stacked': toolbarStacked }]">
                    <!-- Multi select for choosing visible columns -->
                    <MultiSelect
                        v-model="selectedColumns"
                        :options="columns"
                        optionLabel="header"
                        display="comma"
                        placeholder="Select Columns"
                        :maxSelectedLabels="4"
                        selectedItemsLabel="{0} columns selected"
                        :showToggleAll="true"
                    />

                    <!-- Snapshot CRUD control -->
                    <SnapshotPicker v-if="showSnap"
                        :documentQuery="dataSource"
                        :snapshotState="snapshotState"
                        @snapshotChanged="snapshotChanged"
                        stateStorage="session"
                        :stateKey="dataSource"
                    />

                    <!-- Match Any/All Operator -->
                    <Dropdown v-if="showFilter"
                        v-model="selectedTopLevelOperator"
                        :options="topLevelOperators"
                        optionLabel="label"
                        optionValue="value"
                    />
                    <div v-if="!! smartClientCriteria" style="height: 50px; padding-top: 16px; text-align: center;">
                        <span class="pi pi-exclamation-triangle" />
                        &nbsp;
                        <span>Snapshot cannot be displayed or updated</span>
                    </div>
                    <!-- New record - at the end of the toolbar -->
                    <Button v-if="showAdd"
                        class="skyve-vue-list-grid-new"
                        icon="pi pi-plus"
                        label="New"
                        title="New record"
                        :disabled="disableAdd"
                        @click="newRecord"
                    />
                </div>
            </template>
            <!-- No loading overlay - the rows are replaced when the fetch completes -->
            <!-- Only say there are no items once a fetch has completed -->
            <template #empty>
                <span v-if="loading">&nbsp;</span>
                <span v-else-if="canAdd">
                    No Items to show. Click <span class="pi pi-plus" /> to add a new Item.
                </span>
                <span v-else>No Items to show.</span>
            </template>
            <Column
                v-for="col of visibleColumns"
                :key="col.field"
                :field="col.field"
                :header="col.header"
                :sortable="col.sortable"
                :maxConstraints="20"
                :footer="summaryRow[col.field]"
                :filterMatchModeOptions="matchModes[col.type]"
                :headerStyle="columnStyle(col)"
                :bodyStyle="columnStyle(col)"
            >
                <template
                    #filter="{ filterModel }"
                    v-if="showFilter && col.filterable"
                >
                    <span v-if="col.type == 'boolean'">
                        <label :for="'bool-' + col.field">{{ col.header }}</label>
                        <TriStateCheckbox
                            :inputId="'bool-' + col.field"
                            v-model="filterModel.value"
                        />
                    </span>
                    <Dropdown
                        v-else-if="col.type == 'enum'"
                        v-model="filterModel.value"
                        :options="col.enumValues"
                        optionLabel="label"
                        optionValue="value"
                    >
                    </Dropdown>
                    <DateOnlyCalendar
                        v-else-if="col.type == 'date'"
                        v-model="filterModel.value"
                        :dateFormat="dateFormat"
                    />
                    <Calendar
                        v-else-if="col.type == 'dateTime'"
                        v-model="filterModel.value"
                        :dateFormat="dateFormat"
                        showTime
                        :hourFormat="hourFormat"
                    />
                    <Calendar
                        v-else-if="col.type == 'timestamp'"
                        v-model="filterModel.value"
                        :dateFormat="dateFormat"
                        showTime
                        :hourFormat="hourFormat"
                        showSeconds
                        :stepSecond="5"
                    />
                    <TimeCalendar
                        v-else-if="col.type == 'time'"
                        v-model="filterModel.value"
                        :hourFormat="hourFormat"
                    />
                    <InputText
                        v-else-if="['text', 'numeric'].includes(col.type)"
                        v-model="filterModel.value"
                        type="text"
                        class="p-column-filter"
                        :placeholder="'Search by ' + col.header"
                    />
                    <div v-else>
                        Unknown type: {{ col.type }}
                    </div>
                </template>
                <template #body="{ data, field }">
                    <span v-if="col.type == 'image'">
                        <Image
                            :id="data[field]"
                            :module="data.bizModule"
                            :document="data.bizDocument"
                            :binding="field"
                            :width="imageWidth(col)"
                            :height="imageHeight(col)"
                            :emptyThumbnailRelativeFile="col.emptyThumbnailRelativeFile"
                        />
                    </span>
                    <span v-else-if="col.type == 'link'">
                        <a v-if="!!data[field]"
                            :href="contentUrl(data, field)"
                            style="border:0"
                            target="_blank">Content</a>
                    </span>
                    <!-- Unescaped columns are sanitised on the server -->
                    <span v-else-if="col.escape === false" v-html="data[field]" />
                    <span v-else>
                        {{ data[field] }}
                    </span>
                </template>
            </Column>
            <!-- Column key and field here to ensure the session state is saved correctly -->
            <!-- The width !important ensures that resize gestures don't change the column size (60px as per the faces data table) -->
            <Column key="_action"
                    field="_action"
                    :reorderableColumn="false"
                    headerClass="skyve-vue-list-grid-action"
                    bodyClass="skyve-vue-list-grid-action"
                    style="width:60px !important">
                <!-- Final column with the Zoom In controls (New is in the toolbar) -->
                <template #header>
                    <!-- a muted indicator that this column opens the record -->
                    <span v-if="showZoom"
                        class="pi pi-chevron-right skyve-vue-list-grid-zoom-indicator"
                        aria-hidden="true" />
                </template>
                <template #body="{ data }">
                    <Button v-if="showZoom"
                        icon="pi pi-chevron-right"
                        title="View Detail"
                        :disabled="disableZoom"
                        @click="() => highlightRow(data) && zoomInto(data.bizId, data.bizModule, data.bizDocument)"
                    />
                </template>
            </Column>
            <!-- Fetch spinner at the end of the paginator row - complements the faces ajax status spinner -->
            <template #paginatorend>
                <span
                    :class="['skyve-vue-list-grid-spinner', { 'skyve-vue-list-grid-spinner-active': fetching }]"
                    :title="fetching ? 'Loading' : null"
                    aria-live="polite"
                >
                    <!-- the same icon as the faces ajax status spinner (font awesome is loaded by the faces templates) -->
                    <i class="fa-solid fa-circle-notch fa-spin" aria-hidden="true" />
                </span>
            </template>
            <!-- Summary (aggregate) selector at the start of the paginator row -->
            <!-- Always rendered (even without the summary) so the page controls stay centred - see the paginator CSS -->
            <template #paginatorstart>
                <Dropdown v-if="showSummary"
                    :pt:wrapper:style="{ maxHeight: 'none' }"
                    v-model="summarySelection"
                    :options="summaryOpts"
                    placeholder="Summary"
                    :title="summarySelection ? `Summary: ${summarySelection}` : 'Summary'"
                />
            </template>
            <!--
                List views (with a sticky header) also have a floating New button that stays at the
                bottom of the grid's visible area while scrolling and rests above the paginator.
                It is in the (otherwise unused) footer, between the rows and the paginator.
            -->
            <template #footer v-if="showAdd && stickyHeaderAnchorSelector">
                <div :class="['skyve-vue-list-grid-fab', { 'skyve-vue-list-grid-fab-hidden': fetching }]">
                    <Button icon="pi pi-plus"
                        rounded
                        title="New record"
                        aria-label="New record"
                        :disabled="disableAdd"
                        @click="newRecord"
                    />
                </div>
            </template>
        </DataTable>
    </div>
</template>
<style scoped>
/*
 * Density - match the faces data table (compact cells, 38x30 icon buttons) rather than
 * PrimeVue Lara's roomier defaults. Lara's rules are in @layer primevue so these win
 * (themes can still override with !important, eg the editorial theme's cell padding).
 */
.skyve-vue-list-grid :deep(.p-datatable-thead > tr > th),
.skyve-vue-list-grid :deep(.p-datatable-tbody > tr > td),
.skyve-vue-list-grid :deep(.p-datatable-tfoot > tr > td) {
    padding: 0.25rem 0.5rem;
}

.skyve-vue-list-grid :deep(.p-datatable .p-button.p-button-icon-only) {
    width: 2.375rem;
    height: 1.875rem;
    min-height: 0;
    padding: 0;
}

.skyve-vue-list-grid :deep(.p-datatable-header) {
    padding: 0.5rem;
}

.skyve-vue-list-grid :deep(.p-datatable-header .p-button:not(.p-button-icon-only)) {
    padding: 0.5rem 0.75rem;
}

.skyve-vue-list-grid :deep(.p-datatable-header .p-dropdown-label),
.skyve-vue-list-grid :deep(.p-datatable-header .p-multiselect-label),
.skyve-vue-list-grid :deep(.p-paginator .p-dropdown-label) {
    padding: 0.5rem 0.75rem;
}

.skyve-vue-list-grid :deep(.p-paginator) {
    padding: 0.25rem 0.5rem;
}

/* Rows that zoom when clicked (no selection) still show they are clickable */
.skyve-vue-list-grid-zoomable :deep(.p-datatable-tbody > tr) {
    cursor: pointer;
}

/* The muted indicator in the zoom column header */
.skyve-vue-list-grid-zoom-indicator {
    opacity: 0.35;
}

/* New is the last toolbar control - pushed to the right end when on one row */
.skyve-vue-list-grid-toolbar > .skyve-vue-list-grid-new {
    margin-left: auto;
}

.skyve-vue-list-grid-toolbar.skyve-vue-list-grid-toolbar-stacked > .skyve-vue-list-grid-new,
.skyve-vue-list-grid-toolbar.skyve-vue-list-grid-toolbar-measuring > .skyve-vue-list-grid-new {
    margin-left: 0;
}

/*
 * Floating New button (list views) - in the footer, which is made a zero height sticky element so
 * it sticks to the bottom of the viewport while the grid fills it and rests above the paginator at
 * the end of the grid. It sits left of the zoom column so it doesn't cover the rows' zoom buttons.
 */
.skyve-vue-list-grid :deep(.p-datatable-footer) {
    position: sticky;
    bottom: 1rem;
    height: 0;
    padding: 0;
    border: 0;
    background: transparent;
    z-index: 4;
}

.skyve-vue-list-grid-fab {
    display: flex;
    justify-content: flex-end;
    /* the zoom column's width (60px) and a gap - px so it holds whatever the theme's root font size */
    padding-right: 68px;
    pointer-events: none;
}

/* (more specific than the compact icon only button sizing above as it is inside the data table) */
.skyve-vue-list-grid :deep(.p-datatable .skyve-vue-list-grid-fab > .p-button.p-button-icon-only),
.skyve-vue-list-grid-fab > .p-button {
    transition: opacity 0.15s, visibility 0.15s;
    width: 3rem;
    height: 3rem;
    transform: translateY(calc(-100% - 0.5rem));
    border-radius: 50%;
    pointer-events: auto;
    box-shadow: 0 4px 12px rgba(0, 0, 0, 0.25);
}

/* hidden while the grid is loading */
.skyve-vue-list-grid-fab.skyve-vue-list-grid-fab-hidden > .p-button {
    opacity: 0;
    visibility: hidden;
}

/* The action (zoom) column is narrow and centred like the faces data table's */
.skyve-vue-list-grid :deep(.skyve-vue-list-grid-action) {
    text-align: center;
}

.skyve-vue-list-grid :deep(.skyve-vue-list-grid-action .p-column-header-content) {
    justify-content: center;
}

/*
 * Paginator layout - see layoutPaginator().
 * The whole row stays on one line while it fits; centred if it can be.
 */
.skyve-vue-list-grid :deep(.p-paginator) {
    justify-content: center;
    row-gap: 0.25rem;
}

.skyve-vue-list-grid :deep(.p-paginator-left-content),
.skyve-vue-list-grid :deep(.p-paginator-right-content) {
    display: flex;
    align-items: center;
    margin: 0;
}

.skyve-vue-list-grid :deep(.p-paginator-left-content) {
    justify-content: flex-start;
}

.skyve-vue-list-grid :deep(.p-paginator-right-content) {
    justify-content: flex-end;
}

/* centred - the start and end contents take equal widths so the page controls are centred in the grid */
.skyve-vue-list-grid-pager-centred :deep(.p-paginator-left-content),
.skyve-vue-list-grid-pager-centred :deep(.p-paginator-right-content) {
    flex: 1 1 0;
    min-width: max-content;
}

/* uncentred - the start and end contents share the spare width; the page controls stay together between them */
.skyve-vue-list-grid-pager-uncentred :deep(.p-paginator-left-content),
.skyve-vue-list-grid-pager-uncentred :deep(.p-paginator-right-content) {
    flex: 1 1 auto;
}

/*
 * stacked - the page links are centred on the first row; the start (summary), rows per page
 * and end (spinner) contents share the second row (left, centre and right).
 * The ::after is a full width (zero height) item that forces the line break between the rows.
 */
.skyve-vue-list-grid-pager-stacked :deep(.p-paginator)::after {
    content: '';
    flex-basis: 100%;
    height: 0;
    order: 1;
}

.skyve-vue-list-grid-pager-stacked :deep(.p-paginator-left-content) {
    order: 2;
    flex: 1 1 0;
}

.skyve-vue-list-grid-pager-stacked :deep(.p-paginator-rpp-options) {
    order: 3;
}

.skyve-vue-list-grid-pager-stacked :deep(.p-paginator-right-content) {
    order: 4;
    flex: 1 1 0;
}

/*
 * compact - for very narrow grids: the page links are just ‹ 1 / 3 › centred on the first row
 * (with the spinner at the right edge), then the summary and rows per page are stacked full
 * width below, like the stacked toolbar.
 */
.skyve-vue-list-grid-pager-compact :deep(.p-paginator) {
    position: relative;
    justify-content: center;
}

.skyve-vue-list-grid-pager-compact :deep(.p-paginator)::after {
    content: '';
    flex-basis: 100%;
    height: 0;
    order: 1;
}

.skyve-vue-list-grid-pager-compact :deep(.p-paginator-current) {
    white-space: nowrap;
    height: 2rem;
    min-width: 0;
}

.skyve-vue-list-grid-pager-compact :deep(.p-paginator-left-content) {
    order: 2;
    flex: 1 1 100%;
}

.skyve-vue-list-grid-pager-compact :deep(.p-paginator-left-content > *) {
    flex: 1 1 auto;
    min-width: 0;
    margin: 0;
}

.skyve-vue-list-grid-pager-compact :deep(.p-paginator-rpp-options) {
    order: 3;
    flex: 1 1 100%;
    margin: 0;
}

/* the spinner sits at the right of the first row without pushing the page links off centre */
.skyve-vue-list-grid-pager-compact :deep(.p-paginator-right-content) {
    position: absolute;
    top: 0.25rem;
    right: 0.5rem;
}

/*
 * Header toolbar - one row while the controls fit, otherwise stacked full width (see layoutToolbar()).
 * Controls can shrink (their labels truncate) so nothing spills past the grid's edge.
 */
.skyve-vue-list-grid :deep(.p-datatable-header) {
    overflow: hidden;
}

.skyve-vue-list-grid-toolbar {
    display: flex;
    flex-direction: row;
    gap: 0.5rem;
}

.skyve-vue-list-grid-toolbar > * {
    flex: 0 1 auto;
    min-width: 0;
}

.skyve-vue-list-grid-toolbar.skyve-vue-list-grid-toolbar-stacked {
    flex-direction: column;
}

/* measure the controls at their natural widths in a row */
.skyve-vue-list-grid-toolbar.skyve-vue-list-grid-toolbar-measuring {
    flex-direction: row !important;
}

.skyve-vue-list-grid-toolbar.skyve-vue-list-grid-toolbar-measuring > * {
    flex-shrink: 0 !important;
}

.skyve-vue-list-grid-toolbar :deep(.p-button-label) {
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis;
}

/* The fetch spinner keeps its space so the paginator doesn't move when it shows */
.skyve-vue-list-grid-spinner {
    display: inline-flex;
    align-items: center;
    justify-content: center;
    width: 2rem;
    height: 2rem;
    margin-left: 0.5rem;
    color: var(--primary-color);
    font-size: 1.25rem;
    opacity: 0;
    transition: opacity 0.15s;
}

.skyve-vue-list-grid-spinner-active {
    opacity: 1;
}

/* The grid only scrolls horizontally - the page scrolls it vertically */
.skyve-vue-list-grid :deep(.p-datatable-wrapper) {
    overflow-x: auto !important;
    overflow-y: hidden !important;
}

/*
 * See positionStickyHeader() - the header cells are moved down to stay below the
 * anchor elements (eg the top bar) while the page scrolls.
 */
.skyve-vue-list-grid-sticky :deep(.p-datatable-thead > tr > th) {
    transform: translateY(var(--skyve-vue-list-grid-sticky-offset, 0px));
    position: relative;
    z-index: 2;
}

/* Keep the header above positioned content in the rows (eg buttons) - z-index on the cells alone isn't enough */
.skyve-vue-list-grid-sticky :deep(.p-datatable-thead) {
    z-index: 3;
}

/*
 * Where supported, a scroll driven animation moves the header in step with the page scroll.
 * The wrapper's view timeline starts when its top reaches the anchor elements and the header
 * moves down 1px for every pixel scrolled until it reaches the bottom of the table.
 */
@supports (animation-timeline: view()) {
    .skyve-vue-list-grid-sticky.skyve-vue-list-grid-scroll-driven :deep(.p-datatable-wrapper) {
        view-timeline-name: --skyve-vue-list-grid;
        view-timeline-axis: block;
        view-timeline-inset: var(--skyve-vue-list-grid-sticky-top, 0px) 0px;
    }

    .skyve-vue-list-grid-sticky.skyve-vue-list-grid-scroll-driven :deep(.p-datatable-thead > tr > th) {
        animation: skyve-vue-list-grid-sticky linear both;
        animation-timeline: --skyve-vue-list-grid;
        animation-range: exit-crossing 0px exit-crossing var(--skyve-vue-list-grid-sticky-max, 0px);
    }
}

@keyframes skyve-vue-list-grid-sticky {
    from {
        transform: translateY(0px);
    }
    to {
        transform: translateY(var(--skyve-vue-list-grid-sticky-max, 0px));
    }
}
</style>
<style>
/*
 * PrimeVue's Lara theme hard-codes its blue (#3B82F6) into components rather than using
 * --primary-color. Use the page theme's --primary-color instead (eg ultima, ecuador and the
 * editorial palette via its vue.css), so the grid and its overlays match the rest of the page.
 * These are global (not scoped) as overlays are appended to the body; .p-* elements only come
 * from the Vue list grid. Templates can still override them (eg editorial's !important rules).
 */
.skyve-vue-list-grid .p-button:not(.p-button-text):not(.p-button-outlined):not(.p-button-secondary),
.p-column-filter-overlay .p-button:not(.p-button-text):not(.p-button-outlined):not(.p-button-secondary),
.p-dialog .p-button:not(.p-button-text):not(.p-button-outlined):not(.p-button-secondary) {
    background: var(--primary-color);
    border-color: var(--primary-color);
    color: var(--primary-color-text);
}

.skyve-vue-list-grid .p-button:not(.p-button-text):not(.p-button-outlined):not(.p-button-secondary):not(:disabled):hover,
.p-column-filter-overlay .p-button:not(.p-button-text):not(.p-button-outlined):not(.p-button-secondary):not(:disabled):hover,
.p-dialog .p-button:not(.p-button-text):not(.p-button-outlined):not(.p-button-secondary):not(:disabled):hover {
    background: color-mix(in srgb, var(--primary-color) 85%, black);
    border-color: color-mix(in srgb, var(--primary-color) 85%, black);
}

.p-column-filter-overlay .p-button.p-button-outlined,
.p-column-filter-overlay .p-button.p-button-text {
    color: var(--primary-color);
    border-color: var(--primary-color);
}

/* highlights - selected row, current page, chosen list items, active sort and filter */
.skyve-vue-list-grid .p-datatable .p-datatable-tbody > tr.p-highlight,
.skyve-vue-list-grid .p-paginator .p-paginator-pages .p-paginator-page.p-highlight,
.skyve-vue-list-grid .p-column-filter-menu-button.p-column-filter-menu-button-active,
.p-dropdown-panel .p-dropdown-items .p-dropdown-item.p-highlight,
.p-multiselect-panel .p-multiselect-items .p-multiselect-item.p-highlight,
.p-column-filter-overlay .p-column-filter-row-items .p-column-filter-row-item.p-highlight {
    background: color-mix(in srgb, var(--primary-color) 12%, white);
    color: color-mix(in srgb, var(--primary-color) 75%, black);
}

.skyve-vue-list-grid .p-datatable .p-sortable-column.p-highlight,
.skyve-vue-list-grid .p-datatable .p-sortable-column.p-highlight .p-sortable-column-icon {
    color: var(--primary-color);
}

.skyve-vue-list-grid .p-datatable .p-sortable-column .p-sortable-column-badge {
    background: color-mix(in srgb, var(--primary-color) 12%, white);
    color: color-mix(in srgb, var(--primary-color) 75%, black);
}

.p-checkbox .p-checkbox-box.p-highlight,
.p-tristatecheckbox .p-checkbox-box.p-highlight {
    background: var(--primary-color);
    border-color: var(--primary-color);
}

/* focus rings */
.skyve-vue-list-grid .p-button:focus-visible,
.skyve-vue-list-grid .p-dropdown.p-focus,
.skyve-vue-list-grid .p-multiselect.p-focus,
.skyve-vue-list-grid .p-paginator button:focus-visible,
.p-column-filter-overlay .p-inputtext:enabled:focus,
.p-column-filter-overlay .p-button:focus-visible {
    box-shadow: 0 0 0 0.2rem color-mix(in srgb, var(--primary-color) 30%, transparent);
    border-color: var(--primary-color);
}
</style>
