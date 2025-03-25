package org.skyve.persistence;

import jakarta.annotation.Nonnull;

public interface PagedQuery {
	public @Nonnull PagedQuery setFirstResult(int first);
	public @Nonnull PagedQuery setMaxResults(int max);
}
