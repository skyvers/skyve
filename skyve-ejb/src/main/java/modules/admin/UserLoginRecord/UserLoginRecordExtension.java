package modules.admin.UserLoginRecord;

import modules.admin.domain.UserLoginRecord;

public class UserLoginRecordExtension extends UserLoginRecord {
	private static final long serialVersionUID = 200018857407163578L;

	@Override
	public String getBizKey() {
		if (Boolean.TRUE.equals(getFailed())) {
			return "Failed Login attempt: " + super.getBizKey();
		}

		return super.getBizKey();
	}
}
