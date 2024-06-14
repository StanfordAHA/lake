.PHONY: lakespec clean

lakespec:
	@echo "Upgrading pip..."
	@pip install --upgrade pip

	@echo "Installing dumpy..."
	@pip install dumpy

	@echo "Checking for mflowgen repository..."
	@if [ ! -d "./mflowgen" ]; then \
		echo "mflowgen not found, cloning..."; \
		git clone https://github.com/mflowgen/mflowgen.git ./mflowgen; \
	fi

	@echo "Installing mflowgen..."
	@cd ./mflowgen && pip install -e .

	# @echo "Deactivating virtual environment..."
	# @deactivate

	@echo "Checking for lakespec_flow/build directory..."
	@if [ ! -d "./lakespec_flow/build" ]; then \
		echo "Creating lakespec_flow/build directory..."; \
		mkdir -p ./lakespec_flow/build; \
	fi

	@echo "Changing directory to lakespec_flow/build..."
	@cd ./lakespec_flow/build && \
		echo "Running mflowgen from build directory..." && \
		mflowgen run --design ../design

clean-lakespec:
	@echo "Cleaning up..."
	@rm -rf ./mflowgen
