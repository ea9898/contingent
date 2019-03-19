package moscow.ptnl.metrics;

class LoggerDataHolder {
	
	private static final InheritableThreadLocal<TreeCallStackLogger> holder = new InheritableThreadLocal<>();

	static TreeCallStackLogger startRequest() {
		holder.set(new TreeCallStackLogger());
		return holder.get();
	}

	static TreeCallStackLogger getCurrentRequest() {
		return holder.get();
	}

	static void endRequest() {
		holder.set(null);
	}
}
