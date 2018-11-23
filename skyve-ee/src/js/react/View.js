import {Component} from 'react';
import axios from 'axios';

export class View extends Component {
	fetchQuery(module, query, startRow, endRow) {
		const params = {_operationType: 'fetch',
							_dataSource: module + '_' + query,
							_startRow: startRow,
							_endRow: endRow};
		return axios.get('/skyve/smartlist', {params: params})
					.then(res => res.data.response.data);
	}

	fetchModel(module, document, model, startRow, endRow) {
		const params = {_operationType: 'fetch',
							_dataSource: module + '_' + document + '__' + model,
							_startRow: startRow,
							_endRow: endRow};
		return axios.get('/skyve/smartlist', {params: params})
					.then(res => res.data.response.data);
	}

	edit(module, document, bizId) {
		const params = {_operationType: 'fetch',
						_ecnt: 0,
						_ccnt: 0,
						_mod: module,
						_doc: document};
		if (bizId) {
			params.bizId = bizId;
		}
		return axios.get('/skyve/smartedit', {params: params})
					.then(res => res.data.response.data);		
	}
	
	change(name, e) {
		const prop = {};
		prop[name] = e.target.value;
		this.setState(prop)
	}
}
