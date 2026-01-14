Setting up the Conda Environment

This project uses a Conda environment defined in the file conda_env.yml, which is located in this directory.
Follow the steps below to install Conda and create the environment.

1. Install Miniconda (recommended)

	If you do not already have Conda installed, install Miniconda, a minimal Conda distribution.

	Go to the Miniconda download page:
	https://docs.conda.io/en/latest/miniconda.html

	On Windows, it is recommended to:
	- Install “Just for me”
	- Allow the installer to initialize Conda

	After installation, open a new terminal (or Anaconda Prompt on Windows).
	Verify the installation:

	conda --version

2. Create the Conda environment

	Navigate to the project directory containing conda_env.yml:

	cd path/to/project

	Create the environment from the YAML file:

	conda env create -f conda_env.yml

	This will create the environment with all required dependencies.

3. Activate the environment

	Activate the newly created environment:

	conda activate SV_AEM_T2P

	To confirm activation:

	conda env list

	The active environment will be marked with *.

4. Deactivating the environment

	When finished working:

	conda deactivate