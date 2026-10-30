// Lara theme in the primevue cascade layer - see theme.css
import "./theme.css";
import "primevue/resources/primevue.min.css";
import "primeflex/primeflex.css";
import "primeicons/primeicons.css";

import { createApp, h, reactive } from 'vue'
import ListGrid from './ListGrid.vue'
import TimeCalendar from './TimeCalendar.vue'
import DateOnlyCalendar from './DateOnlyCalendar.vue'
import SnapshotPicker from './SnapshotPicker.vue'
import Image from './Image.vue'

import PrimeVue from 'primevue/config'

import DataTable from 'primevue/datatable';
import Column from 'primevue/column';
import Button from 'primevue/button'
import InputText from 'primevue/inputtext'
import MultiSelect from 'primevue/multiselect';
import TriStateCheckbox from 'primevue/tristatecheckbox';
import Calendar from 'primevue/calendar';
import Dropdown from 'primevue/dropdown';
import TieredMenu from 'primevue/tieredmenu';
import Dialog from 'primevue/dialog';
import ContextMenu from "primevue/contextmenu";

// SKYVE name space definition
window.SKYVE ??= {};

// The mounted list grids by container id: { app, container, props }
const grids = new Map();

/**
 * Two grid configurations are for the same grid if they show the same data with the same columns.
 */
function sameGrid(a, b) {
    return a.owningModule === b.owningModule &&
            a.owningDocument === b.owningDocument &&
            a.drivingModule === b.drivingModule &&
            a.drivingDocument === b.drivingDocument &&
            a.query === b.query &&
            a.model === b.model &&
            JSON.stringify(a.columns) === JSON.stringify(b.columns);
}

/**
 * Unmount grids whose containers have been removed from the page and not reused.
 */
function sweepDetachedGrids() {
    for (const [id, grid] of grids) {
        if (!grid.container.isConnected) {
            grid.app.unmount();
            grids.delete(id);
            if (window[id] === grid.app) {
                delete window[id];
            }
        }
    }
}

function mount(gridConfig, container) {
    alignFont();
    const props = reactive({ ...gridConfig, renderedAt: Date.now() });
    const grid = createApp({
        render: () => h(ListGrid, props)
    });
    grid.use(PrimeVue, { ripple: true });
    grid.component('Button', Button);
    grid.component('Column', Column);
    grid.component('DataTable', DataTable);
    grid.component('InputText', InputText);
    grid.component('MultiSelect', MultiSelect);
    grid.component('TriStateCheckbox', TriStateCheckbox);
    grid.component('Calendar', Calendar);
    grid.component('Dropdown', Dropdown);
    grid.component('TieredMenu', TieredMenu);
    grid.component('TimeCalendar', TimeCalendar);
    grid.component('DateOnlyCalendar', DateOnlyCalendar);
    grid.component('SnapshotPicker', SnapshotPicker);
    grid.component('Dialog', Dialog);
    grid.component('ContextMenu', ContextMenu);
    grid.component('Image', Image);

    grid.mount(container);

    grids.set(gridConfig.containerId, { app: grid, container, props });
    // the current (reactive) configuration is available for debugging and tests
    grid.skyveConfig = props;
    window[gridConfig.containerId] = grid;
}

/**
 * Create (or refresh) a list grid.
 *
 * This is called when the grid's container is rendered - on page load and whenever the
 * container is re-rendered by a faces ajax update.
 * When re-rendered, the existing grid (and its state - page, sort, filters, selection etc)
 * is put back in place of the new (empty) container straight away and its data is
 * refreshed in the background, like a faces data table rendered with its rows.
 */
/**
 * PrimeVue's Lara theme uses its own font (Inter). Unless the page has remapped PrimeVue's
 * --font-family (eg the editorial template's vue.css), use the page's font so the grid and its
 * overlays (which are appended to the body) match the rest of the page (eg the ultima and ecuador templates).
 */
function alignFont() {
    const root = document.documentElement;
    if (root.dataset.skyveVueFont) {
        return;
    }
    root.dataset.skyveVueFont = 'true';
    const font = getComputedStyle(root).getPropertyValue('--font-family');
    if (font.includes('Inter var')) {
        root.style.setProperty('--font-family', getComputedStyle(document.body).fontFamily);
    }
}

window.SKYVE.listgrid = function (gridConfig) {
    const id = gridConfig.containerId;
    // containerId is the container's client id; fall back to an id suffix match for older callers
    const target = document.getElementById(id) ?? document.querySelector("[id$='" + id + "']");
    const existing = grids.get(id);

    if (existing && target && (existing.container !== target) &&
            (!existing.container.isConnected) && sameGrid(existing.props, gridConfig)) {
        target.replaceWith(existing.container);
        const { columns, ...rest } = gridConfig; // columns are the same - keep the existing objects
        Object.assign(existing.props, rest, { renderedAt: Date.now() });
        setTimeout(sweepDetachedGrids, 1000);
        return;
    }

    // A different grid or first render - mount once the container has finished rendering
    setTimeout(() => {
        const current = grids.get(id);
        if (current) {
            current.app.unmount();
            grids.delete(id);
        }
        const container = document.getElementById(id) ?? document.querySelector("[id$='" + id + "']");
        if (container) {
            mount(gridConfig, container);
        }
        sweepDetachedGrids();
    }, 0);
}
