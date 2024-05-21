import "primevue/resources/themes/lara-light-blue/theme.css";
import "primevue/resources/primevue.min.css";
import "primeflex/primeflex.css";
import "primeicons/primeicons.css";

import { createApp } from 'vue'
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

// JS create method
window.SKYVE.listgrid = function ({module, title, document, query, columns, targetSelector}) {

    const grid = createApp(ListGrid, {
        module,
        title,
        query,
        columns,
        document
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

    grid.mount(targetSelector);
}  
