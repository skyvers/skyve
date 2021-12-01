package org.skyve.metadata.model.document.fluent;

import org.skyve.impl.metadata.model.document.AbstractInverse;
import org.skyve.impl.metadata.model.document.InverseMany;

public class FluentInverseMany extends FluentInverse<FluentInverseMany> {
	private InverseMany inverse = null;
	
	public FluentInverseMany() {
		inverse = new InverseMany();
	}

	public FluentInverseMany(InverseMany inverse) {
		this.inverse = inverse;
	}

	public FluentInverseMany from(@SuppressWarnings("hiding") InverseMany inverse) {
		super.from(inverse);
		return this;
	}
	
	@Override
	public AbstractInverse get() {
		return inverse;
	}
}
