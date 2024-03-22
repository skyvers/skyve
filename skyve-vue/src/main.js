import "primevue/resources/themes/lara-light-teal/theme.css";

import { createApp } from 'vue'
import ListGrid from './ListGrid.vue'

import PrimeVue from 'primevue/config'

import DataTable from 'primevue/datatable';
import Column from 'primevue/column';
import Button from 'primevue/button'
import InputText from 'primevue/inputtext'
import MultiSelect from 'primevue/multiselect';
import TriStateCheckbox from 'primevue/tristatecheckbox';

// SKYVE name space definition
if (!window.SKYVE) window.SKYVE = {};

// JS create method
window.SKYVE.listgrid = function (config) {

    const grid = createApp(ListGrid, {
        'module': config.m,
        'title': config.t,
        'query': config.q,
        'columns': config.c
    });
    grid.use(PrimeVue, { ripple: true });
    grid.component('Button', Button);
    grid.component('Column', Column);
    grid.component('DataTable', DataTable);
    grid.component('InputText', InputText);
    grid.component('MultiSelect', MultiSelect);
    grid.component('TriStateCheckbox', TriStateCheckbox);

    //    grid.configure(config); - can't call the method exposed here
    grid.mount(config.e);
}  
