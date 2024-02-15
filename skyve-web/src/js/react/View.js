import {Component} from 'react';
import axios from 'axios';

export class View extends Component {
	fetchQuery(module, query, startRow, endRow) {
		const params = {_operationType: 'fetch',
							_dataSource: module + '_' + query,
							_startRow: startRow,
							_endRow: endRow};
		return axios.get('/skyve/smartlist', {params: params})
					.then(res => processResponse(res))
					.catch(error => View.processError(error));
	}

	fetchModel(module, document, model, startRow, endRow) {
		const params = {_operationType: 'fetch',
							_dataSource: module + '_' + document + '__' + model,
							_startRow: startRow,
							_endRow: endRow};
		return axios.get('/skyve/smartlist', {params: params})
					.then(res => processResponse(res))
					.catch(error => View.processError(error));
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
					.then(res => processResponse(res))	
					.catch(error => View.processError(error));
	}
	
	change(name, e) {
		const prop = {};
		prop[name] = e.target.value;
		this.setState(prop)
	}
	
	static processError(error) {
		if (error.response) {
			console.log(error.response.data);
			console.log(error.response.status);
			console.log(error.response.headers);
		}
		else if (error.request) {
			console.log(error.request);
		}
		else {
			// maybe error.message
			console.log(error);
		}
	}
}

function processResponse(res) {
	// Skyve's smart service servlet filter returns login page if unauthorized
console.log(res.status);
console.log(res.headers['location']);
/*
	if (res.headers['content-type'] === 'text/html;charset=UTF-8') {
		res.status = 401;
		throw { ...res };
	}
*/
    return res.data.response.data;
}
