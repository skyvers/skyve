import React, {Component} from 'react';
import PropTypes from 'prop-types';

export class VBox extends Component {
	static propTypes = {
		children: PropTypes.node.isRequired
	}

	render() {
		return (
			<div className="p-grid p-dir-col">
				{this.props.children}
			</div>
		);
	}
}
export class HBox extends VBox {
	render() {
		return (
			<div className="p-grid">
				{this.props.children}
			</div>
		);
	}
}
export class Form extends VBox {
}
export class Cell extends Component {
	static propTypes = {
		children: PropTypes.node.isRequired
	}

	render() {
		return (
			<div className="p-col">
				{this.props.children}
			</div>
		);
	}
}