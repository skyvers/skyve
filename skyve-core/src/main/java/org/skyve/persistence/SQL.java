package org.skyve.persistence;

import java.util.List;

import org.apache.commons.beanutils.DynaBean;
import org.locationtech.jts.geom.Geometry;
import org.skyve.domain.Bean;
import org.skyve.domain.types.DateOnly;
import org.skyve.domain.types.DateTime;
import org.skyve.domain.types.Decimal;
import org.skyve.domain.types.Enumeration;
import org.skyve.domain.types.TimeOnly;
import org.skyve.domain.types.Timestamp;
import org.skyve.metadata.model.Attribute.AttributeType;

import jakarta.annotation.Nonnull;
import jakarta.annotation.Nullable;

/**
 * 
 */
public interface SQL extends BeanQuery, ScalarQuery, TupleQuery, DMLQuery {
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable DateOnly value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable DateTime value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable TimeOnly value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Timestamp value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Decimal value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Integer value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Long value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable String value, boolean memoOrMarkup);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Bean value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Geometry value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Boolean value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Enumeration value);
	@Nonnull SQL putParameter(@Nonnull String name, @Nullable Object value, @Nonnull AttributeType type);
	
	int getTimeoutInSeconds();
	void setTimeoutInSeconds(int timeoutInSeconds);
	@Nonnull SQL noTimeout();
	
	@Nonnull List<DynaBean> dynaResults();
	@Nullable DynaBean dynaResult();
	@Nonnull DynaBean retrieveDyna();
	@Nonnull AutoClosingIterable<DynaBean> dynaIterable();
}
