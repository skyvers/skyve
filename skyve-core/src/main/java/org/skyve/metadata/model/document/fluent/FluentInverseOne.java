package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.InverseOne;

public class FluentInverseOne extends FluentInverse<FluentInverseOne> {
	private InverseOne inverse = null;
	
	public FluentInverseOne() {
		inverse = new InverseOne();
	}

	public FluentInverseOne(InverseOne inverse) {
		this.inverse = inverse;
	}

	public FluentInverseOne from(@SuppressWarnings("hiding") InverseOne inverse) {
		super.from(inverse);
		return this;
	}
	
	@Override
	public AbstractInverse get() {
		return inverse;
	}
}
