package org.skyve.metadata.view.model.chart;

import javax.xml.bind.annotation.XmlTransient;

/**
 * Bucket data by a number of defined ranges.
 * 
 * @author mike
 */
@XmlTransient
public class NumericRangeBucket implements Bucket {
	private static final long serialVersionUID = 6925394592182937839L;

	protected int[] ranges;

	/**
	 * The ranges parameter specifies the breaking points in category values.
	 * That is ranges of 0,10,20 means ranges -infinity->0,0->10,10->20,20->+infinity
	 * @param ranges The range breaking points.
	 */
	public NumericRangeBucket(int... ranges) {
		this.ranges = ranges;
	}
	
	@Override
	public String bizQLExpression(String categoryBindingOrAlias) {
		StringBuilder result = new StringBuilder(256);
		for (int i = 0, l = ranges.length; i < l; i++) {
			if (i == 0) {
				result.append("case when ").append(categoryBindingOrAlias).append(" < ").append(ranges[0]);
				result.append(" then 0");
			}
			if (i > 0) {
				result.append(" when ").append(categoryBindingOrAlias).append(" between ");
				result.append(ranges[i - 1]).append(" and ").append(ranges[i]);
				result.append(" then ").append(i);
			}
			if (i == (l - 1)) {
				result.append(" else ").append(i + 1).append(" end");
			}
		}
		return result.toString();
	}
	
	@Override
	public String label(Object category) {
		if (category instanceof Number) {
			int index = ((Number) category).intValue();
			if (index == 0) {
				return "<" + ranges[0];
			}
			else if (index == ranges.length) {
				return ">" + ranges[index - 1];
			}
			else {
				return ranges[index - 1] + "-" + ranges[index];
			}
		}
		return null;
	}
}
