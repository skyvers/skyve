package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.InverseOne;

public class FluentInverseOne extends FluentInverse<FluentInverseOne> {
	private InverseOne inverse = new InverseOne();
	
	public FluentInverseOne() {
		// nothing to see
	}

	public FluentInverseOne(InverseOne inverse) {
		super(inverse);
	}
	
	@Override
	public AbstractInverse get() {
		return inverse;
	}
}
