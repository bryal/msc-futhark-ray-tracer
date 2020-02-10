#include <inttypes.h>

void load_obj_data(
    char* obj_path,
    size_t* num_tris, size_t* num_mat_components,
    float** tri_data, uint32_t** tri_mats, float** mat_data);
void free_obj_data(float* tri_data, uint32_t* tri_mats, float* mat_data);
