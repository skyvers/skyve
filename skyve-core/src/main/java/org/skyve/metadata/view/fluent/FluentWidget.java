package org.skyve.metadata.view.fluent;

import org.skyve.impl.metadata.view.AbsoluteSize;
import org.skyve.impl.metadata.view.AbsoluteWidth;
import org.skyve.impl.metadata.view.ConstrainableHeight;
import org.skyve.impl.metadata.view.ConstrainableSize;
import org.skyve.impl.metadata.view.MinimumHeight;
import org.skyve.impl.metadata.view.RelativeSize;
import org.skyve.impl.metadata.view.RelativeWidth;
import org.skyve.impl.metadata.view.ResponsiveWidth;
import org.skyve.impl.metadata.view.container.Box;
import org.skyve.metadata.MetaData;

abstract class FluentWidget {
	protected FluentWidget() {
		// nothing to see
	}
	
	public abstract MetaData get();
	
	protected static void absoluteWidth(AbsoluteWidth from, FluentAbsoluteWidth<?> to) {
		Integer i = from.getPixelWidth();
		if (i != null) {
			to.pixelWidth(i.intValue());
		}
	}
	
	protected static void absoluteSize(AbsoluteSize from, FluentAbsoluteSize<?> to) {
		absoluteWidth(from, to);
		Integer i = from.getPixelHeight();
		if (i != null) {
			to.pixelHeight(i.intValue());
		}
	}

	protected static void minimumHeight(MinimumHeight from, FluentMinimumHeight<?> to) {
		Integer i = from.getMinPixelHeight();
		if (i != null) {
			to.minPixelHeight(i.intValue());
		}
	}
	
	protected static void constrainableHeight(ConstrainableHeight from, FluentConstrainableHeight<?> to) {
		minimumHeight(from, to);
		Integer i = from.getMaxPixelHeight();
		if (i != null) {
			to.maxPixelHeight(i.intValue());
		}
	}

	protected static void constrainableSize(ConstrainableSize from, FluentConstrainableSize<?> to) {
		constrainableHeight(from, to);
		Integer i = from.getMinPixelWidth();
		if (i != null) {
			to.minPixelWidth(i.intValue());
		}
		i = from.getMaxPixelWidth();
		if (i != null) {
			to.maxPixelWidth(i.intValue());
		}
	}

	protected static void relativeWidth(RelativeWidth from, FluentRelativeWidth<?> to) {
		absoluteWidth(from, to);
		
		Integer i = from.getPercentageWidth();
		if (i != null) {
			to.percentageWidth(i.intValue());
		}
		i = from.getResponsiveWidth();
		if (i != null) {
			to.responsiveWidth(i.intValue());
		}
	}

	protected static void responsiveWidth(ResponsiveWidth from, FluentRelativeSize<?> to) {
		relativeWidth(from, to);
		
		Integer i = from.getSm();
		if (i != null) {
			to.sm(i.intValue());
		}
		i = from.getMd();
		if (i != null) {
			to.md(i.intValue());
		}
		i = from.getLg();
		if (i != null) {
			to.lg(i.intValue());
		}
		i = from.getXl();
		if (i != null) {
			to.xl(i.intValue());
		}

	}

	protected static void relativeSize(RelativeSize from, FluentRelativeSize<?> to) {
		responsiveWidth(from, to);
		constrainableSize(from, to);

		Integer i = from.getPixelHeight();
		if (i != null) {
			to.pixelHeight(i.intValue());
		}
		i = from.getPercentageHeight();
		if (i != null) {
			to.percentageHeight(i.intValue());
		}
	}
	
	protected static void box(Box from, FluentBox<?> to) {
		relativeSize(from, to);
		to.shrinkWrap(from.getShrinkWrap());
		Integer i = from.getPixelPadding();
		if (i != null) {
			to.pixelPadding(i.intValue());
		}
		i = from.getPixelMemberPadding();
		if (i != null) {
			to.pixelMemberPadding(i.intValue());
		}
	}
}
